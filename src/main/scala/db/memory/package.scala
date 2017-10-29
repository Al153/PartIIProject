package db

import core.containers.Operation
import core.error.E
import core.intermediate.Find
import core.intermediate.unsafe._
import db.common._
import db.interfaces.DBInstance
import schema.{RelationName, SchemaObject, TableName}
import utils._

import scala.collection.immutable.Queue
import scalaz.Scalaz._
import scalaz._

/**
  * Created by Al on 20/10/2017.
  *
  * An in-memory database executor
  */
package object memory {
  type MemoryTree = Map[TableName, MemoryTable]
  type RelatedPair = (MemoryObject, MemoryObject)

  implicit class MemoryTreeOps(memoryTree: MemoryTree) {
    def findObj(findable: UnsafeFindable): E \/ Vector[MemoryObject] = for {
      table <- memoryTree.get(findable.tableName).fold(\/.left[E, MemoryTable](MissingTableName(findable.tableName)))(_.right)

      res <- table.find(findable)
    } yield res
  }

  def matches(o: MemoryObject, p: UnsafeFindable): Boolean = ??? // check that the object matches the pattern



  def findSingleImpl(t: UnsafeFindSingle, tree: MemoryTree): E \/ Vector[MemoryObject] = {
    def recurse(t: UnsafeFindSingle) = findSingleImpl(t, tree)

    t match {
      case USFind(pattern) => tree.get(pattern.tableName).fold(\/.left[E, Vector[MemoryObject]](MissingTableName(pattern.tableName)))(_.find(pattern))
      case USFrom(start, rel) => for {
        left <- recurse(start)
        res <- findPairsImpl(rel, left, tree).map(v => v.map(_._2))
      } yield res
      case USNarrowS(start, pattern) => for {
        broad <- recurse(start)
      } yield broad.filter(matches(_, pattern)) // todo: this should be more typesafe
    }
  }

  def findSingleSetImpl(t: UnsafeFindSingle, tree: MemoryTree): E \/ Set[MemoryObject] = {
    def recurse(t: UnsafeFindSingle) = findSingleSetImpl(t, tree)

    t match {
      case USFind(pattern) => tree.get(pattern.tableName).fold(\/.left[E, Set[MemoryObject]](MissingTableName(pattern.tableName)))(_.find(pattern).map(_.toSet))
      case USFrom(start, rel) => for {
        left <- recurse(start)
        res <- findPairsSetImpl(rel, left, tree).map(v => v.map(_._2))
      } yield res
      case USNarrowS(start, pattern) => for {
        broad <- recurse(start)
      } yield broad.filter(matches(_, pattern)) // todo: this should be more typesafe
    }
  }

  def findPairsImpl(q: UnsafeFindPair, left: Vector[MemoryObject], tree: MemoryTree): E \/ Vector[RelatedPair] = {
    def recurse(t: UnsafeFindPair, left: Vector[MemoryObject]) = findPairsImpl(t, left, tree)

    q match {
      case USAnd(l, r) => for {
        leftRes <- recurse(l, left)
        rightRes <- recurse(r, left)
      } yield leftRes.intersect(rightRes)
      case USOr(l, r) => for {
        leftRes <- recurse(l, left)
        rightRes <- recurse(r, left)
      } yield leftRes.union(rightRes)

      case USChain(l, r) => for {
        lres <- recurse(l, left)
        rres <- recurse(r, lres.map(_._2))
      } yield join(lres, rres)

      case USId => left.map(x => (x, x)).right

      case USNarrow(l, p) => for {
        broad <- recurse(l, left)
      } yield broad.filter(pair => matches(pair._2, p))

      case USRel(rel) =>
          EitherOps.sequence(left.map {
            leftObject: MemoryObject => {
              val relatedPatterns = leftObject.getRelated(rel.name).toVector
              val eRelatedObjects = EitherOps.sequence(relatedPatterns.map(f => tree.findObj(f)))
              val res = eRelatedObjects.map(relatedObjects => relatedObjects.flatten.map((leftObject, _)))
              res
            }
          }).map(_.flatten)


      case USRevRel(rel) =>
        EitherOps.sequence(left.map {
          leftObject: MemoryObject => {
            val relatedPatterns = leftObject.getRevRelated(rel.name).toVector
            val eRelatedObjects = EitherOps.sequence(relatedPatterns.map(f => tree.findObj(f)))
            val res = eRelatedObjects.map(relatedObjects => relatedObjects.flatten.map((leftObject, _)))
            res
          }
        }).map(_.flatten)

      case USUpto(n, rel) =>
        if (n <= 0) left.map(x => (x, x)).right
        else for {
          lres <- recurse(rel, left)
          rres <- fixedPoint(left => findPairsSetImpl(rel, left, tree), lres.mapProj2.toSet.mapPair, Number(n))
        } yield join(lres, rres.toVector)
      case USBetween(low, high, rel) => recurse(USChain(USExactly(low, rel), USUpto(high - low, rel)), left)
      case USAtleast(n, rel) =>
        if (n > 0) {
          recurse(USChain(USExactly(n, rel), USAtleast(0, rel)), left)
        } else {
          // otherwise find a fixed point
          for {
            res <- fixedPoint(left => findPairsSetImpl(rel, left, tree), left.toSet.mapPair, NoLimit)
          } yield res.toVector
        }
      case USExactly(n, rel) => if (n <= 0) {
        left.map(x => (x, x)).right
      } else {
        recurse(USChain(rel, USExactly(n-1, rel)), left) // todo: this is probably quite slow
      }
    }
  }

  def findPairsSetImpl(t: UnsafeFindPair, left: Set[MemoryObject], tree: MemoryTree): E \/ Set[RelatedPair] = {
    def recurse(t: UnsafeFindPair, left: Set[MemoryObject]) = findPairsSetImpl(t, left, tree)
    t match {
      case USAnd(l, r) => for {
        leftRes <- recurse(l, left)
        rightRes <- recurse(r, left)
      } yield leftRes.intersect(rightRes)
      case USOr(l, r) => for {
        leftRes <- recurse(l, left)
        rightRes <- recurse(r, left)
      } yield leftRes.union(rightRes)

      case USChain(l, r) => for {
        lres <- recurse(l, left)
        rres <- recurse(r, lres.map(_._2))
      } yield joinSet(lres, rres)

      case USId => left.map(x => (x, x)).right

      case USNarrow(l, p) => for {
        broad <- recurse(l, left)
      } yield broad.filter(pair => matches(pair._2, p))

      case USRel(rel) =>
        EitherOps.sequence(left.map {
          leftObject: MemoryObject => {
            val relatedPatterns = leftObject.getRelated(rel.name).toVector
            val eRelatedObjects = EitherOps.sequence(relatedPatterns.map(f => tree.findObj(f)))
            val res = eRelatedObjects.map(relatedObjects => relatedObjects.flatten.map((leftObject, _)))
            res
          }
        }).map(_.flatten)

      case USRevRel(rel) =>
        EitherOps.sequence(left.map {
          leftObject: MemoryObject => {
            val relatedPatterns = leftObject.getRevRelated(rel.name).toVector
            val eRelatedObjects = EitherOps.sequence(relatedPatterns.map(f => tree.findObj(f)))
            val res = eRelatedObjects.map(relatedObjects => relatedObjects.flatten.map((leftObject, _)))
            res
          }
        }).map(_.flatten)

      case USUpto(n, rel) =>
        if (n <= 0) left.map(x => (x, x)).right
        else for {
          lres <- recurse(rel, left)
          rres <- fixedPoint(left => findPairsSetImpl(rel, left, tree), lres.mapProj2.map(x => (x, x)), Number(n))
        } yield joinSet(lres, rres)
      case USBetween(low, high, rel) => recurse(USChain(USExactly(low, rel), USUpto(high - low, rel)), left)
      case USAtleast(n, rel) =>
        if (n > 0) {
          recurse(USChain(USExactly(n, rel), USAtleast(0, rel)), left)
        } else {
          // otherwise find a fixed point
          for {
            res <- fixedPoint(left => findPairsSetImpl(rel, left, tree), left.map(x => (x, x)), NoLimit)
          } yield res
        }
      case USExactly(n, rel) => if (n <= 0) {
        left.map(x => (x, x)).right
      } else {
        recurse(USChain(rel, USExactly(n-1, rel)), left) // todo: this is probably quite slow
      }
    }
  }

  def fixedPoint(searchStep: Set[MemoryObject] => E \/ Set[RelatedPair], initial: Set[RelatedPair], limit: Limit): E \/ Set[RelatedPair] = {
    def aux(pairsToExplore: Set[RelatedPair], alreadyExplored: Set[MemoryObject], acc: Set[RelatedPair], limit: Limit): E \/ Set[RelatedPair] = for {
      foundPairs <- searchStep(pairsToExplore.mapProj2)
      newPairs: Set[RelatedPair] = notExplored(foundPairs, alreadyExplored)
      newAcc: Set[RelatedPair] = acc | joinSet(acc, newPairs)

      res <- limit match {
        case Number(n) =>
          if (shouldContinue(newPairs, n))
            newAcc.right
          else {
            val newExplored = alreadyExplored | newPairs.mapProj1
            aux(newPairs, newExplored, newAcc, Number(n-1))
          }

        case NoLimit =>
          if (newPairs.isEmpty)
            newAcc.right
          else {
            val newExplored = alreadyExplored | newPairs.mapProj1
            aux(newPairs, newExplored, newAcc, NoLimit)
          }

      }
    } yield res

    limit match {
      case Number(0) => initial.right
      case _ => aux(initial, initial.mapProj1, initial, limit)
    }
  }

  // Filter out the unexplored pairs
  private def notExplored(found: Set[RelatedPair], explored: Set[MemoryObject]): Set[RelatedPair] = found.filter {case (_, r) => !explored.contains(r)}

  // Determine whether to continue

  private def shouldContinue(newPairs: Set[RelatedPair], n: Int): Boolean = newPairs.isEmpty || n <= 1

  // slow join

  def join(leftRes: Vector[RelatedPair], rightRes: Vector[RelatedPair]): Vector[RelatedPair] =
    for {
      (from, to) <- leftRes
      right <- rightRes.collect {case (f, t) if f == to => t}
    } yield (from, right)

  def joinSet(leftRes: Set[RelatedPair], rightRes: Set[RelatedPair]): Set[RelatedPair] =
    for {
      (from, to) <- leftRes
      right <- rightRes.collect {case (f, t) if f == to => t}
    } yield (from, right)


  // Breadth first == dijkstra's

  def allShortestPathsImpl(start: Set[MemoryObject], searchStep: MemoryObject => E \/ Set[RelatedPair]): E \/ Set[MemoryPath] = {
    def aux(fringe: Queue[MemoryPath], alreadyExplored: Set[MemoryObject], acc: Set[MemoryPath]): E \/ Set[MemoryPath] = {
      for {
        stepResult <- doStep(searchStep, fringe, alreadyExplored)

        (newFringe, path, objects) = stepResult

        newExplored = alreadyExplored | objects
        newAcc = acc | objects.map(path + _)
        res <-
        if (newFringe.isEmpty)
          newAcc.right
        else
          aux(newFringe, newExplored, newAcc)
      } yield res
    }

    aux(toQueue(start.map(MemoryPath.apply)), Set(), Set())
  }

  // Step function, fringe => NewFringe, pickedPath, newlyFound
  private def doStep(searchStep: MemoryObject => E \/ Set[RelatedPair], fringe: Queue[MemoryPath], alreadyExplored: Set[MemoryObject]): E \/ (Queue[MemoryPath], MemoryPath, Set[MemoryObject]) =
    if (fringe.nonEmpty) {
      val top = fringe.head
      val newFringe = fringe.tail

      for {
        next <- searchStep(top.getLast)
        newObjects = next.mapProj2.diff(alreadyExplored)
      } yield (newFringe, top, newObjects)
    } else {
      EmptyFringeError.left // todo return some error
    }

  private def toQueue[A](s: Set[A]): Queue[A] = Queue(s.toSeq: _*) // todo: is this fast?


  def singleShortestsPathImpl(start: Set[MemoryObject], end: UnsafeFindable, searchStep: MemoryObject => E \/ Set[RelatedPair]): E \/ Option[MemoryPath] = {
    def aux(fringe: Queue[MemoryPath], alreadyExplored: Set[MemoryObject], acc: Set[MemoryPath]): E \/ Option[MemoryPath] = {
      for {
        stepResult <- doStep(searchStep, fringe, alreadyExplored)
        (newFringe, path, objects) = stepResult
        res <- objects.find(matches(_, end)) match {
          case None =>
            val newExplored = alreadyExplored | objects
            val newAcc = acc | objects.map(path + _)
            if (newFringe.isEmpty) None.right // return no result
            else aux(newFringe, newExplored, newAcc)
          case Some(o) => (path + o).some.right
        }
      } yield res
    }

    aux(toQueue(start.map(MemoryPath.apply)), Set(), Set())
  }


  def find[A](a: A, t: MemoryTree)(implicit sa: SchemaObject[A]): E \/ Set[MemoryObject] = findSingleImpl(Find(sa.findable(a)).getUnsafe, t).map(_.toSet)


  def write[A](t: MemoryTree)(tableName1: TableName, memoryObject1: UnsafeFindable, relationName: RelationName, tableName2: TableName, memoryObject2: UnsafeFindable): E \/ MemoryTree = {
    for {
      table1 <- t.get(tableName1).fold(\/.left[E, MemoryTable](MissingTableName(tableName1)))(_.right)
      table2 <- t.get(tableName2).fold(\/.left[E, MemoryTable](MissingTableName(tableName2)))(_.right)

      o1 <- table1.findOrWrite(memoryObject1)
      updatedO1 =  o1.map(_.addRelation(relationName, memoryObject2))
      o2 <- table2.findOrWrite(memoryObject2)
      updatedO2 = o2.map(_.addReverseRelation(relationName, memoryObject1))


      res = t + (tableName1 -> table1.insert(updatedO1), tableName2 -> table2.insert(updatedO2))
    } yield res
  }

}
