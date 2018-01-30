package impl.memory.methods

import core.backend.intermediate.Find
import core.backend.intermediate.unsafe._
import core.user.schema.SchemaObject
import core.utils._
import core.utils.algorithms.FixedPointTraversal
import impl.memory.errors.{MemoryMissingRelation, MemoryMissingTableName}
import impl.memory.{MemoryEither, MemoryObject, MemoryTree, RelatedPair}

import scalaz.Scalaz._

/**
  * Implements lookup queries for Set commands
  */
trait SetImpl { self: ExecutorMethods with Joins with RepetitionImpl =>
  /**
    * Find an A in the tree
    */
  def find[A](a: A, t: MemoryTree)
             (implicit sa: SchemaObject[A]): MemoryEither[Set[MemoryObject]] =
    for {
      unsafeQuery <- Find(sa.findable(a)).getUnsafe(instance.schema).leftMap(MemoryMissingRelation)
      res <- findSingleSetImpl(unsafeQuery, t)
    } yield res

  /**
    * Implements findSingleSet, by recursing over the ADT
    * @param t - query
    * @param tree - [[MemoryTree]] to execute against
    */

  def findSingleSetImpl(t: UnsafeFindSingle, tree: MemoryTree): MemoryEither[Set[MemoryObject]] = {
    def recurse(t: UnsafeFindSingle) = findSingleSetImpl(t, tree)
    t match {
      case USFind(pattern) =>
        tree
          .getOrError(pattern.tableName, MemoryMissingTableName(pattern.tableName))
          .flatMap(_.find(pattern).map(_.toSet))
      case USFrom(start, rel) => for {
        left <- recurse(start)
        res <- findPairsSetImpl(rel, left, tree).map(_.mapProj2)
      } yield res
      case USAndS(left, right) => for {
        r1 <- recurse(left)
        r2 <- recurse(right)
      } yield r1 intersect r2

      case USOrS(left, right) => for {
        r1 <- recurse(left)
        r2 <- recurse(right)
      } yield r1 union r2
    }
  }
  /**
    * Implements findPairsSet, by recursing over the ADT
    * @param t - query
    * @param left - the objects from which relation should start. This minimises the amount of work that has to be done
    * @param tree - [[MemoryTree]] to execute against
    *
    * Most cases are fairly straight forward
    */

  def findPairsSetImpl(t: UnsafeFindPair, left: Set[MemoryObject], tree: MemoryTree): MemoryEither[Set[RelatedPair]] = {
    def recurse(t: UnsafeFindPair, left: Set[MemoryObject]) = findPairsSetImpl(t, left, tree)
    t match {
      case USAnd(l, r) => for {
        leftRes <- recurse(l, left)
        rightRes <- recurse(r, left)
      } yield leftRes.intersect(rightRes)


      case USAndRight(l, r) => for {
        leftRes <- recurse(l, left)
        rightRes <- findSingleSetImpl(r, tree)
      } yield leftRes.filter{case (a, b) => rightRes.contains(b)}

      case USAndLeft(l, r) => for {
        leftRes <- recurse(l, left)
        rightRes <- findSingleSetImpl(r, tree)
      } yield leftRes.filter{case (a, b) => rightRes.contains(a)}


      case USOr(l, r) => for {
        leftRes <- recurse(l, left)
        rightRes <- recurse(r, left)
      } yield leftRes.union(rightRes)

      case USChain(l, r) => for {
        lres <- recurse(l, left)
        rres <- recurse(r, lres.map(_._2))
      } yield joinSet(lres, rres)

      case USDistinct(r) => for {
        rres <- recurse(r, left)
      } yield rres.filter{case (a, b) => a != b}

      case USId(_) => left.map(x => (x, x)).right

      case USRel(rel) =>
        left.map(_.getRelatedMemoryObjects(rel, tree)).flattenE

      case USRevRel(rel) =>
        left.map(_.getRevRelatedMemoryObjects(rel, tree)).flattenE

      case USUpto(n, rel) =>
        val stepFunction: Set[MemoryObject] => MemoryEither[Set[MemoryObject]] =
          left => findPairsSetImpl(rel, left, tree).map(_.mapProj2)
        upTo(stepFunction, left, n)

      case USFixedPoint(rel) =>
        // find a fixed point
        val stepFunction: Set[MemoryObject] => MemoryEither[Set[MemoryObject]] =
          left => findPairsSetImpl(rel, left, tree).map(_.mapProj2)
        for {
          res <- fixedPoint(stepFunction, left.map(x => (x, x)))
        } yield res

      case USExactly(n, rel) =>
        val stepFunction: MemoryObject => MemoryEither[Set[MemoryObject]] =
          left => findPairsSetImpl(rel, Set(left), tree).map(_.mapProj2)
        FixedPointTraversal.exactly(stepFunction, left, n)

    }
  }
}