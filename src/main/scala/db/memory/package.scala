package db

import core.error.E
import core.intermediate.Find
import core.intermediate.unsafe._
import db.common._
import schema.{RelationName, SchemaDescription, SchemaObject, TableName}
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
    def findPattern(findable: UnsafeFindable): E \/ Vector[MemoryObject] = for {
      table <- memoryTree.getOrError(findable.tableName, MissingTableName(findable.tableName))
      res <- table.find(findable)
    } yield res

    def findObj(tableName: TableName, obj: DBObject): E \/ Option[MemoryObject] = for {
      table <- memoryTree.getOrError(tableName, MissingTableName(tableName))
      res = table.find(obj)
    } yield res
  }

  def matches(o: MemoryObject, p: UnsafeFindable): Boolean = p.matches(o.value) // check that the object matches the pattern



  def findSingleImpl(t: UnsafeFindSingle, tree: MemoryTree): E \/ Vector[MemoryObject] = {
    def recurse(t: UnsafeFindSingle) = findSingleImpl(t, tree)

    t match {
      case USFind(pattern) => tree.getOrError(pattern.tableName, MissingTableName(pattern.tableName)).flatMap(_.find(pattern))
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
      case USFind(pattern) => tree.getOrError(pattern.tableName, MissingTableName(pattern.tableName)).flatMap(_.find(pattern).map(_.toSet))
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
    println(tree)
    def recurse(t: UnsafeFindPair, left: Vector[MemoryObject]) = findPairsImpl(t, left, tree)

    q match {
      case USAnd(l, r) => for {
        leftRes <- recurse(l, left)
        rightRes <- recurse(r, left)
      } yield {
        println("left = " + leftRes)
        println("right = " + rightRes)
        leftRes.intersect(rightRes)
      }

      case USAndSingle(l, r) => for {
        leftRes <- recurse(l, left)
        rightRes <- findSingleSetImpl(r, tree)
      } yield leftRes.filter{case (a, b) => rightRes.contains(b)}

      case USOr(l, r) => for {
        leftRes <- recurse(l, left)
        rightRes <- recurse(r, left)
      } yield leftRes.union(rightRes)

      case USChain(l, r) => for {
        lres <- recurse(l, left)
        rres <- recurse(r, lres.mapProj2.distinct) // reduce double joining
      } yield {
        println("(All) Chain right expr = " + r)
        println("(All) Chain Left result = " + lres)
        println("(All) Chain right Result = " + rres)
        val res = join(lres, rres)
        println("Chain res = " + res)
        res
      }


      case USDistinct(r) => for {
        rres <- recurse(r, left)
      } yield {
        println("Distinct Subexpr = " + r)
        println("Distinct Before distinction = " + rres)
        println("Distinct After distinction = " +  rres.filter{case (a, b) => a != b})
        rres.filter{case (a, b) => a != b}
      }

      case USId => left.map(x => (x, x)).right

      case USNarrow(l, p) => for {
        broad <- recurse(l, left)
      } yield {
        println("(All) Broad = " + broad)
        val res = broad.filter(pair => matches(pair._2, p))
        println("(All) Narrowed = " + res)
        res
      }

      case USRel(rel) =>
          EitherOps.sequence(left.map {
            leftObject: MemoryObject => {
              val related = leftObject.getRelated(rel.name).toVector
              val eRelatedObjects = EitherOps.sequence(related.map(o => tree.findObj(rel.to, o)))
              val res = eRelatedObjects.map(relatedObjects => relatedObjects.flatten.map((leftObject, _)))
              res
            }
          }).map(_.flatten)


      case USRevRel(rel) =>
        println("RevRel Left = " + left)
        EitherOps.sequence(left.map {
          leftObject: MemoryObject => {
            val related = leftObject.getRevRelated(rel.name).toVector
            println("Rev related = " + related.mkString("\n"))
            val eRelatedObjects = EitherOps.sequence(related.map(o => tree.findObj(rel.from, o)))
            val res = eRelatedObjects.map(relatedObjects => relatedObjects.flatten.map((leftObject, _)))
            res
          }
        }).map(_.flatten)

      case USUpto(n, rel) =>
        if (n <= 0) left.map(x => (x, x)).right
        else {
          val stepFunction: Set[MemoryObject] => E \/ Set[RelatedPair] = left => findPairsSetImpl(rel, left, tree)
          for {
            rres <- fixedPoint(stepFunction, left.toSet.mapPair, Number(n))
          } yield rres.toVector
        }

      case USBetween(low, high, rel) => {
        recurse(USChain(USExactly(low, rel), USUpto(high - low, rel)), left)
      }
      case USAtleast(n, rel) =>
        if (n > 0) {
          recurse(USChain(USExactly(n, rel), USAtleast(0, rel)), left)
        } else {
          // otherwise find a fixed point
          println("Atleast: Finding fixed point")
          println("Atleast: Left = " + left.mkString("\n\t\t\t"))
          val stepFunction: Set[MemoryObject] => E \/ Set[RelatedPair] = left => findPairsSetImpl(rel, left, tree)
          for {
            res <- fixedPoint(stepFunction, left.toSet.mapPair, NoLimit)
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
      } yield {
        println("left = " + leftRes)
        println("right = " + rightRes)
        leftRes.intersect(rightRes)
      }

      case USAndSingle(l, r) => for {
        leftRes <- recurse(l, left)
        rightRes <- findSingleSetImpl(r, tree)
      } yield leftRes.filter{case (a, b) => rightRes.contains(b)}

      case USOr(l, r) => for {
        leftRes <- recurse(l, left)
        rightRes <- recurse(r, left)
      } yield leftRes.union(rightRes)

      case USChain(l, r) => for {
        lres <- recurse(l, left)
        rres <- recurse(r, lres.map(_._2))
      } yield {
        println("(Distinct) Chain Left result = " + lres)
        println("(Distinct) Chain right Result = " + rres)
        val res = joinSet(lres, rres)
        println("Chain res = " + res)
        res
      }

      case USDistinct(r) => for {
        rres <- recurse(r, left)
      } yield {
        println("Subexpr = " + r)
        println("Before distinction = " + rres)
        println("After distinction = " +  rres.filter{case (a, b) => a != b})
        rres.filter{case (a, b) => a != b}
      }

      case USId => left.map(x => (x, x)).right

      case USNarrow(l, p) => for {
        broad <- recurse(l, left)
      } yield {
        println("(Distinct) Broad = " + broad)
        val res = broad.filter(pair => matches(pair._2, p))
        println("(Distinct) Narrowed = " + res)
        res
      }

      case USRel(rel) =>
        EitherOps.sequence(left.map {
          leftObject: MemoryObject => {
            val related = leftObject.getRelated(rel.name)
            val eRelatedObjects = EitherOps.sequence(related.map(o => tree.findObj(rel.to, o)))
            val res = eRelatedObjects.map(relatedObjects => relatedObjects.flatten.map((leftObject, _)))
            res
          }
        }).map(_.flatten)

      case USRevRel(rel) =>
        EitherOps.sequence(left.map {
          leftObject: MemoryObject => {
            val related = leftObject.getRevRelated(rel.name)
            val eRelatedObjects = EitherOps.sequence(related.map(o => tree.findObj(rel.from, o)))
            val res = eRelatedObjects.map(relatedObjects => relatedObjects.flatten.map((leftObject, _)))
            res
          }
        }).map(_.flatten)

      case USUpto(n, rel) =>
        if (n <= 0) left.map(x => (x, x)).right
        else for {
          rres <- fixedPoint(left => findPairsSetImpl(rel, left, tree), left.mapPair, Number(n))
        } yield rres

      case USBetween(low, high, rel) => recurse(USChain(USExactly(low, rel), USUpto(high - low, rel)), left)
      case USAtleast(n, rel) =>
        if (n > 0) {
          recurse(USChain(USExactly(n, rel), USAtleast(0, rel)), left)
        } else {
          // otherwise find a fixed point
          println("Atleast: Left = " + left.mkString("\n\t\t\t"))
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

  /**
    *   Find the fixed point of the search step function
    */

  // TODO: this is incorrect - assumes no loops, etc
  // need separate fixed point and non-fixed point algos

  private def fixedPoint(searchStep: Set[MemoryObject] => E \/ Set[RelatedPair], initial: Set[RelatedPair], limit: Limit): E \/ Set[RelatedPair] = {

    /*
     * Algorithm idea:
     *  keep a fringe of pairs to explore, an accumulator of found pairs, and an accumulator of found left hand sides
     *
     *  In each step, find all the pairs that are reachable from each value in the fringe
     *
     *
     */

    def aux(fringe: Set[MemoryObject], alreadyExplored: Set[MemoryObject], acc: Set[RelatedPair], limit: Limit): E \/ Set[RelatedPair] =
      limit match {
        case Number(n) if n <= 0 => acc.right
        case _ =>
          for {
            foundPairs <- searchStep(fringe)

            newRight = foundPairs.mapProj2.diff(alreadyExplored)
            newAcc = acc | joinSet(acc, foundPairs)

            res <-
              if (newRight.isEmpty)
                newAcc.right
              else {
                val newExplored = alreadyExplored | fringe
                aux(newRight, newExplored, newAcc, limit.newLimit)
              }
          } yield res
      }
    aux(initial.mapProj2, Set(), initial, limit)
  }


  // slow join

  private def join(leftRes: Vector[RelatedPair], rightRes: Vector[RelatedPair]): Vector[RelatedPair] = {
    println("\n\n\n\nJoin, left = " + leftRes.mkString("\n\t\t"))
    println("join, right = " + rightRes.mkString("\n\t\t"))

    // build an index of all values to join, prevents overduplication
    val collectedLeft = leftRes.foldLeft(Map[MemoryObject, Vector[MemoryObject]]()) {
      case (m, pair) =>
        m + (pair._2 -> (m.getOrElse(pair._2, Vector[MemoryObject]()) :+ pair._1))
    }

    println("Join, index = " + collectedLeft.mkString("\n\t\t"))

    val res = for {
      (middle, to) <- rightRes
      from <- collectedLeft.getOrElse(middle, Vector())
    } yield (from, to)

    println("Join, res = " + res.mkString("\n\t\t"))
    res
  }


  private def joinSet(leftRes: Set[RelatedPair], rightRes: Set[RelatedPair]): Set[RelatedPair] = {
    println("\n\n\n\nJoin, left = " + leftRes.mkString("\n\t\t"))
    println("join, right = " + rightRes.mkString("\n\t\t"))

    // build an index of all values to join, prevents overduplication
    val collectedLeft = leftRes.foldLeft(Map[MemoryObject, Set[MemoryObject]]()) {
      case (m, pair) =>
        m + (pair._2 -> (m.getOrElse(pair._2, Set[MemoryObject]()) ++ Set(pair._1)))
    }

    println("Join, index = " + collectedLeft.mkString("\n\t\t"))

    val res = for {
      (middle, to) <- rightRes
      from <- collectedLeft.getOrElse(middle, Set())
    } yield (from, to)

    println("Join, res = " + res.mkString("\n\t\t"))
    res
}


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
      val top = fringe.head // pop the top off of the fringe
      for {
        next <- searchStep(top.getLast)
        newObjects = next.mapProj2.diff(alreadyExplored)
        newFringe = fringe.tail ++ newObjects.diff(alreadyExplored).map(top + _) // todo: Probably slow
      } yield (newFringe, top, newObjects)
    } else {
      EmptyFringeError.left
    }

  private def toQueue[A](s: Set[A]): Queue[A] = Queue() ++ s


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


  def find[A](a: A, t: MemoryTree)(implicit sa: SchemaObject[A], sd: SchemaDescription): E \/ Set[MemoryObject] =
    for {
      unsafeQuery <- Find(sa.findable(a)).getUnsafe
      res <- findSingleImpl(unsafeQuery, t).map(_.toSet)
    } yield res


  def write[A](t: MemoryTree, tableName1: TableName, memoryObject1: DBObject, relationName: RelationName, tableName2: TableName, memoryObject2: DBObject): E \/ MemoryTree = {
    if (tableName1 != tableName2) { // different behaviour
      for {
        table1 <- t.getOrError(tableName1, MissingTableName(tableName1))
        table2 <- t.getOrError(tableName2, MissingTableName(tableName2))

        o1 <- table1.findOrWrite(memoryObject1)
        updatedO1 =  o1.addRelation(relationName, memoryObject2)
        o2 <- table2.findOrWrite(memoryObject2)
        updatedO2 = o2.addReverseRelation(relationName, memoryObject1)
        res = t + (tableName1 -> table1.insert(updatedO1), tableName2 -> table2.insert(updatedO2))
      } yield res
    } else { // if same table need to add both to table
      writeSelfRelation(t, tableName1, memoryObject1, relationName, memoryObject2)
    }
  }

  private def writeSelfRelation(t: MemoryTree, tableName1: TableName, memoryObject1: DBObject, relationName: RelationName, memoryObject2: DBObject) =
    for {
      table1 <- t.getOrError(tableName1, MissingTableName(tableName1))

      o1 <- table1.findOrWrite(memoryObject1)
      updatedO1 =  o1.addRelation(relationName, memoryObject2)
      o2 <- table1.findOrWrite(memoryObject2)
      updatedO2 = o2.addReverseRelation(relationName, memoryObject1)

      res = t + (tableName1 -> table1.insert(updatedO1).insert(updatedO2))
    } yield res

}
