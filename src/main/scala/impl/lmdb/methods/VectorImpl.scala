package impl.lmdb.methods

import core.backend.common.algorithms.{FixedPointTraversal, Joins}
import core.intermediate.unsafe._
import core.schema.{SchemaDescription, SchemaObject}
import core.utils.EitherOps
import impl.lmdb.LMDBEither
import impl.lmdb.access.{Commit, ObjId}
import impl.lmdb.errors.LMDBError
import impl.lmdb.tables.impl.ObjectRetrievalTable
import core.utils._
import scalaz._, Scalaz._

/**
  * Created by Al on 30/12/2017.
  */
trait VectorImpl { self: Methods =>
  def findVector[A](
                       ut: UnsafeFindSingle,
                       commits: Set[Commit],
                       objectTable: ObjectRetrievalTable
                     )(implicit extractor: SchemaObject[A]): LMDBEither[Vector[A]] =
    for {
      ids <- findSingleVector(ut, commits)
      res <- objectTable.retrieve(ids)
    } yield res


  def findVectorPairs[A, B](
                           ut: UnsafeFindPair,
                           commits: Set[Commit],
                           leftTable: ObjectRetrievalTable,
                           rightTable: ObjectRetrievalTable
                           )(
    implicit sa: SchemaObject[A],
    sb: SchemaObject[B],
    sd: SchemaDescription
  ): LMDBEither[Vector[(A, B)]] = for {
    initial <- leftTable.lookupVector(commits)
    pairs <- findPairVector(ut, commits, initial)
    res <- extractPairVector[A, B](pairs)
  } yield res


  def extractPairVector[A, B](in: Vector[(ObjId, ObjId)])(
    implicit sa: SchemaObject[A],
    sb: SchemaObject[B]
  ): LMDBEither[Vector[(A, B)]] = ???

  def findSingleVector(ut: UnsafeFindSingle, commits: Set[Commit]): LMDBEither[Vector[ObjId]] = {
    def recurse(t: UnsafeFindSingle) = findSingleVector(t, commits)

    ut match {
      case USFind(pattern) => instance.lookupPattern(pattern, commits).map(_.toVector)
      case USFrom(start, rel) => for {
        left <- recurse(start)
        res <- findPairVector(rel, commits, left)
      } yield res.mapProj2

      case USNarrowS(start, pattern) => for {
        broad <- recurse(start)
        filtered <- instance.lookupPattern(pattern, commits)
      } yield broad.filter(_ in filtered)
    }
  }

  def findPairVector(
                             ut: UnsafeFindPair,
                             commits: Set[Commit],
                             from: Vector[ObjId]
                           ): LMDBEither[Vector[(ObjId, ObjId)]] = {
    def recurse(ut: UnsafeFindPair, from: Vector[ObjId]) = findPairVector(ut, commits, from)
    ut match {
      case USAnd(l, r)  => for {
        a <- recurse(l, from)
        b <- recurse(r, from)
      } yield a intersect b // todo: this is a O(N^2) intersection


      case USAndSingle(l, r) => for {
        leftRes <- recurse(l, from)
        rightRes <- findPairSingle(r, commits) // todo: use a set here
      } yield leftRes.filter{case (a, b) => rightRes.contains(b)}

      case USOr(l, r) => for {
        leftRes <- recurse(l, from)
        rightRes <- recurse(r, from)
      } yield leftRes.union(rightRes)


      case USChain(l, r) => for {
        lres <- recurse(l, from)
        rres <- recurse(r, lres.mapProj2.distinct) // reduce double joining
      } yield Joins.joinVector(lres, rres)

      case USDistinct(r) => for {
        rres <- recurse(r, from)
      } yield rres.filter{case (a, b) => a != b}


      case USId(_) => from.map(x => (x, x)).right

      case USNarrow(l, p) => recurse(USAndSingle(l, USFind(p)), from)


      case USRel(rel) =>
        EitherOps.sequence(from.map {
          leftObject: ObjId => for {
            related <- instance.controlTables.relations.followRelation(leftObject, commits, rel.name)
          } yield related.map((leftObject, _))
        }).map(_.flatten)


      case USRevRel(rel) =>
        EitherOps.sequence(from.map {
          leftObject: ObjId => for {
            related <- instance.controlTables.reverseRelations.followRelation(leftObject, commits, rel.name)
          } yield related.map((leftObject, _))
        }).map(_.flatten)

      case USUpto(n, rel) =>
        // todo: step function should use Set impl
        // todo: fix these castings
        val stepFunction: Set[ObjId] => LMDBEither[Set[ObjId]] = left => recurse(rel, left.toVector).map(_.mapProj2.toSet)
        for {
          rres <- FixedPointTraversal.upTo[LMDBError, ObjId](stepFunction, from.toSet, n)
        } yield rres.toVector

      case USBetween(low, high, rel) => recurse(USChain(USExactly(low, rel), USUpto(high - low, rel)), from)

      case USAtleast(n, rel) =>
        if (n > 0) {
          recurse(USChain(USExactly(n, rel), USAtleast(0, rel)), from)
        } else {
          // otherwise find a fixed point
          // todo: fix these castings
          val stepFunction: Set[ObjId] => LMDBEither[Set[ObjId]] = left => recurse(rel, left.toVector).map(_.mapProj2.toSet)
          for {
            res <- FixedPointTraversal.fixedPoint(stepFunction, from.toSet.mapPair)
          } yield res.toVector
        }
      case USExactly(n, rel) => if (n <= 0) {
        from.map(x => (x, x)).right
      } else {
        recurse(USChain(rel, USExactly(n-1, rel)), from) // todo: this is probably quite slow
      }
    }
  }
}
