package impl.memory.methods

import core.backend.intermediate.Find
import core.backend.intermediate.unsafe._
import core.user.schema.{SchemaDescription, SchemaObject}
import core.utils._
import impl.memory.errors.{MemoryMissingRelation, MemoryMissingTableName}
import impl.memory.{MemoryEither, MemoryObject, MemoryTree, RelatedPair}

import scalaz.Scalaz._

/**
  * Implements lookup queries for Vector commands
  */
trait VectorImpl { self: ExecutorMethods with SetImpl with Joins with RepetitionImpl =>

  /**
    * Find an A in the tree
    */
  def find[A](a: A, t: MemoryTree)
             (implicit sa: SchemaObject[A], sd: SchemaDescription): MemoryEither[Set[MemoryObject]] =
    for {
      unsafeQuery <- Find(sa.findable(a)).getUnsafe.leftMap(MemoryMissingRelation)
      res <- findSingleImpl(unsafeQuery, t)
    } yield res.toSet

  /**
    * Implements findSingleVector, by recursing over the ADT
    * @param t - query
    * @param tree - [[MemoryTree]] to execute against
    */

  def findSingleImpl(t: UnsafeFindSingle, tree: MemoryTree): MemoryEither[Vector[MemoryObject]] = {
    def recurse(t: UnsafeFindSingle) = findSingleImpl(t, tree)

    t match {
      case USFind(pattern) =>
        tree
          .getOrError(pattern.tableName, MemoryMissingTableName(pattern.tableName))
          .flatMap(_.find(pattern)
            .map(_.toVector))
      case USFrom(start, rel) => for {
        left <- recurse(start)
        res <- findPairsImpl(rel, left, tree).map(_.mapProj2)
      } yield res
      case USNarrowS(start, pattern) => for {
        broad <- recurse(start)
      } yield broad.filter(matches(_, pattern)) // todo: this should be more typesafe
    }
  }

  /**
    * Implements findPairsSet, by recursing over the ADT
    * @param q - query
    * @param left - the objects from which relation should start. This minimises the amount of work that has to be done
    * @param tree - [[MemoryTree]] to execute against
    *
    * Most cases are fairly straight forward
    */
  def findPairsImpl(q: UnsafeFindPair, left: Vector[MemoryObject], tree: MemoryTree): MemoryEither[Vector[RelatedPair]] = {
    def recurse(t: UnsafeFindPair, left: Vector[MemoryObject]) = findPairsImpl(t, left, tree)

    q match {
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
        rres <- recurse(r, lres.mapProj2.distinct) // reduce double joining
      } yield join(lres, rres)


      case USDistinct(r) => for {
        rres <- recurse(r, left)
      } yield rres.filter{case (a, b) => a != b}


      case USId(_) => left.map(x => (x, x)).right

      case USRel(rel) =>
        left.map {_.getRelatedMemoryObjects(rel, tree).map(_.toVector)}.flattenE


      case USRevRel(rel) =>
        left.map {_.getRevRelatedMemoryObjects(rel, tree).map(_.toVector)}.flattenE

      case USUpto(n, rel) =>
        val stepFunction: Set[MemoryObject] => MemoryEither[Set[MemoryObject]]
        = left => findPairsSetImpl(rel, left, tree).map(_.mapProj2)
          for {
            rres <- upTo(stepFunction, left.toSet, n)
          } yield rres.toVector

      case USBetween(low, high, rel) =>
        recurse(USChain(USExactly(low, rel), USUpto(high - low, rel)), left)

      case USAtleast(n, rel) =>
        if (n > 0) {
          recurse(USChain(USExactly(n, rel), USAtleast(0, rel)), left)
        } else {
          // otherwise find a fixed point
          val stepFunction: Set[MemoryObject] => MemoryEither[Set[MemoryObject]] =
            left => findPairsSetImpl(rel, left, tree).map(_.mapProj2)
          for {
            res <- fixedPoint(stepFunction, left.toSet.mapPair)
          } yield res.toVector
        }
      case USExactly(n, rel) => if (n <= 0) {
        left.map(x => (x, x)).right
      } else {
        recurse(USChain(rel, USExactly(n-1, rel)), left) // todo: this is probably quite slow
      }
    }
  }
}