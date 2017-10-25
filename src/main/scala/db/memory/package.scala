package db

import core.containers.Operation
import core.error.E
import core.intermediate.unsafe._
import db.common.{Limit, MissingTableName, NoLimit, Number}
import schema.TableName
import utils._

import scalaz.Scalaz._
import scalaz._

/**
  * Created by Al on 20/10/2017.
  *
  * An in-memory database exectutor
  */
package object memory {
  type MemoryTree = Map[TableName, MemoryTable]
  type RelatedPair = (MemoryObject, MemoryObject)

  def matches(o: MemoryObject, p: UnsafeFindable): Boolean = ??? // check that the object matches the pattern
  def readOp[A](f: MemoryTree => E \/ A): Operation[E, A] = ???



  def findSingle(t: UnsafeFindSingle, tree: MemoryTree): E \/ Vector[MemoryObject] = {
    def recurse(t: UnsafeFindSingle) = findSingle(t, tree)

    t match {
      case USFind(pattern) =>
        for {
          broad <- tree.get(pattern.tableName).map(_.objects).fold(\/.left[E, Vector[MemoryObject]](MissingTableName(pattern.tableName)))(_.right[E])
        } yield broad.filter(matches(_, pattern))
      case USFrom(start, rel) => for {
        left <- recurse(start)
        res <- findPairs(rel, left, tree).map(v => v.map(_._2))
      } yield res
      case USNarrowS(start, pattern) => for {
        broad <- recurse(start)
      } yield broad.filter(matches(_, pattern)) // todo: this should be more typesafe
    }
  }

  def findPairs(t: UnsafeFindPair, left: Vector[MemoryObject], tree: MemoryTree): E \/ Vector[RelatedPair] = {
    def recurse(t: UnsafeFindPair, left: Vector[MemoryObject]) = findPairs(t, left, tree)

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
      } yield join(lres, rres)

      case USId => left.map(x => (x, x)).right

      case USNarrow(l, p) => for {
        broad <- recurse(l, left)
      } yield broad.filter(pair => matches(pair._2, p))

      case USRel(rel) => \/-(left.flatMap(o => o.relations.getOrElse(rel.name, Vector()).map((o, _))))

      case USRevRel(rel) => \/-(left.flatMap(o => o.revRelations.getOrElse(rel.name, Vector()).map((o, _))))

      case USUpto(n, rel) =>
        if (n <= 0) left.map(x => (x, x)).right
        else for {
          lres <- recurse(rel, left)
          rres <- fixedPoint(left => findPairsSet(rel, left)(tree), lres.mapProj2.toSet.map(x => (x, x)), Number(n))
        } yield join(lres, rres.toVector)
      case USBetween(low, high, rel) => recurse(USChain(USExactly(low, rel), USUpto(high - low, rel)), left)
      case USAtleast(n, rel) =>
        if (n > 0) {
          recurse(USChain(USExactly(n, rel), USAtleast(0, rel)), left)
        } else {
          // otherwise find a fixed point
          for {
            res <- fixedPoint(left => findPairsSet(rel, left, tree), left.toSet.map(x => (x, x)), NoLimit)
          } yield res.toVector
        }
      case USExactly(n, rel) => if (n <= 0) {
        left.map(x => (x, x)).right
      } else {
        recurse(USChain(rel, USExactly(n-1, rel)), left) // todo: this is probably quite slow
      }
    }
  }

  def findPairsSet(t: UnsafeFindPair, left: Set[MemoryObject], tree: MemoryTree): E \/ Set[RelatedPair] = {
    def recurse(t: UnsafeFindPair, left: Set[MemoryObject]) = findPairsSet(t, left, tree)
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

      case USRel(rel) => \/-(left.flatMap(o => o.relations.getOrElse(rel.name, Vector()).map((o, _))))

      case USRevRel(rel) => \/-(left.flatMap(o => o.revRelations.getOrElse(rel.name, Vector()).map((o, _))))

      case USUpto(n, rel) =>
        if (n <= 0) left.map(x => (x, x)).right
        else for {
          lres <- recurse(rel, left)
          rres <- fixedPoint(left => findPairsSet(rel, left, tree), lres.mapProj2.map(x => (x, x)), Number(n))
        } yield joinSet(lres, rres)
      case USBetween(low, high, rel) => recurse(USChain(USExactly(low, rel), USUpto(high - low, rel)), left)
      case USAtleast(n, rel) =>
        if (n > 0) {
          recurse(USChain(USExactly(n, rel), USAtleast(0, rel)), left)
        } else {
          // otherwise find a fixed point
          for {
            res <- fixedPoint(left => findPairsSet(rel, left, tree), left.map(x => (x, x)), NoLimit)
          } yield res
        }
      case USExactly(n, rel) => if (n <= 0) {
        left.map(x => (x, x)).right
      } else {
        recurse(USChain(rel, USExactly(n-1, rel)), left) // todo: this is probably quite slow
      }
    }
  }

  def fixedPoint(searchStep: (Set[MemoryObject]) => E \/ Set[RelatedPair], initial: Set[RelatedPair], limit: Limit): E \/ Set[RelatedPair] = {
    def aux(pairsToExplore: Set[RelatedPair], alreadyExplored: Set[MemoryObject], acc: Set[RelatedPair], limit: Limit): E \/ Set[RelatedPair] = for {
      foundPairs <- searchStep(pairsToExplore)
      newPairs = foundPairs.filter {case (_, r) => !alreadyExplored.contains(r)}
      newAcc = acc | joinSet(acc, newPairs)
      res <- limit match {
        case Number(n) => if (newPairs.isEmpty || n <= 1) newAcc.right else aux(newPairs, alreadyExplored | newPairs.map(_._1), newAcc, Number(n-1))
        case NoLimit => if (newPairs.isEmpty) newAcc.right else aux(newPairs, alreadyExplored | newPairs.mapProj1, newAcc, NoLimit)
      }
    } yield res

    limit match {
      case Number(0) => initial.right
      case _ => aux(initial, initial.mapProj1, initial, limit)
    }
  }

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





}
