package impl.lmdb.methods

import core.backend.common.algorithms.{FixedPointTraversal, Joins}
import core.backend.intermediate.unsafe._
import core.user.schema.{SchemaDescription, SchemaObject}
import core.utils.EitherOps
import impl.lmdb.LMDBEither
import impl.lmdb.access.{Commit, ObjId}
import impl.lmdb.errors.{LMDBError, MissingIndex, LMDBMissingTable}
import impl.lmdb.tables.impl.ObjectRetrievalTable
import core.utils._

import scalaz._
import Scalaz._

/**
  * Created by Al on 30/12/2017.
  */
trait SetImpl { self: Methods =>
  def readSet[A](
                     ut: UnsafeFindSingle,
                     commits: Set[Commit],
                     objectTable: ObjectRetrievalTable
                   )(implicit extractor: SchemaObject[A], sd: SchemaDescription): LMDBEither[Set[A]] = {
    for {
      ids <- findSingleSet(ut, commits)
      res <- objectTable.retrieve(ids)
    } yield res
  }

  def readSetPairs[A, B](
                          ut: UnsafeFindPair,
                          commits: Set[Commit],
                          leftTable: ObjectRetrievalTable,
                          rightTable: ObjectRetrievalTable
                        )(
    implicit sa: SchemaObject[A],
    sb: SchemaObject[B],
    sd: SchemaDescription
  ): LMDBEither[Set[(A, B)]] = for {
    initial <- leftTable.lookup(commits)
    pairs <- findPairSet(ut, commits, initial)
    res <- extractPairSet[A, B](pairs)
  } yield res

  def extractPairSet[A, B](in: Set[(ObjId, ObjId)])(
    implicit sa: SchemaObject[A],
    sb: SchemaObject[B]
  ): LMDBEither[Set[(A, B)]] = for {
    aTable <- instance.objects.getOrError(sa.name, LMDBMissingTable(sa.name))
    bTable <- instance.objects.getOrError(sb.name, LMDBMissingTable(sb.name))

    leftIds = in.mapProj1.toSet
    rightIds = in.mapProj2.toSet

    leftIndex <- EitherOps.sequence(leftIds.map {
      id => aTable.retrieve[A](id).map(id -> _)
    }).toMapE

    rightIndex <- EitherOps.sequence(rightIds.map {
      id => bTable.retrieve[B](id).map(id -> _)
    }).toMapE

    res <- EitherOps.sequence(
      in.map {
        case (leftId, rightId) =>
          for {
            a <- leftIndex.getOrError(leftId, MissingIndex(leftId, leftIndex))
            b <- rightIndex.getOrError(rightId, MissingIndex(rightId, rightIndex))
          } yield (a, b)
      }
    )
  } yield res

  def findSingleSet(ut: UnsafeFindSingle, commits: Set[Commit]): LMDBEither[Set[ObjId]] = {
    def recurse(t: UnsafeFindSingle) = findSingleSet(t, commits)

    ut match {
      case USFind(pattern) => instance.lookupPattern(pattern, commits)
      case USFrom(start, rel) => for {
        left <- recurse(start)
        res <- findPairSet(rel, commits, left)
      } yield res.mapProj2
      case USNarrowS(start, pattern) => for {
        broad <- recurse(start)
        filtered <- instance.lookupPattern(pattern, commits)
      } yield broad.filter(_ in filtered)
    }
  }

  protected def findPairSet(
                             ut: UnsafeFindPair,
                             commits: Set[Commit],
                             from: Set[ObjId]
                           ): LMDBEither[Set[(ObjId, ObjId)]] = {
    def recurse(ut: UnsafeFindPair, from: Set[ObjId]): LMDBEither[Set[(ObjId, ObjId)]] = findPairSet(ut, commits, from)
    ut match {
      case USAnd(l, r)  => for {
        a <- recurse(l, from)
        b <- recurse(r, from)
      } yield a intersect b


      case USAndRight(l, r) => for {
        leftRes <- recurse(l, from)
        rightRes <- findSingleSet(r, commits)
      } yield leftRes.filter{case (a, b) => rightRes.contains(b)}

      case USAndLeft(l, r) => for {
        leftRes <- recurse(l, from)
        rightRes <- findSingleSet(r, commits) // todo: use a set here
      } yield leftRes.filter{case (a, b) => rightRes.contains(a)}


      case USOr(l, r) => for {
        leftRes <- recurse(l, from)
        rightRes <- recurse(r, from)
      } yield leftRes.union(rightRes)


      case USChain(l, r) => for {
        lres <- recurse(l, from)
        rres <- recurse(r, lres.mapProj2) // reduce double joining
      } yield Joins.joinSet(lres, rres)

      case USDistinct(r) => for {
        rres <- recurse(r, from)
      } yield rres.filter{case (a, b) => a != b}


      case USId(_) => from.map(x => (x, x)).right

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
        val stepFunction: Set[ObjId] => LMDBEither[Set[ObjId]] = left => recurse(rel, left).map(_.mapProj2)

        FixedPointTraversal.upTo[LMDBError, ObjId](stepFunction, from, n)


      case USBetween(low, high, rel) => recurse(USChain(USExactly(low, rel), USUpto(high - low, rel)), from)

      case USAtleast(n, rel) =>
        if (n > 0) {
          recurse(USChain(USExactly(n, rel), USAtleast(0, rel)), from)
        } else {
          // otherwise find a fixed point
          val stepFunction: Set[ObjId] => LMDBEither[Set[ObjId]] = left => recurse(rel, left).map(_.mapProj2)
          FixedPointTraversal.fixedPoint(stepFunction, from.mapPair)
        }


      case USExactly(n, rel) => if (n <= 0) {
        from.map(x => (x, x)).right
      } else {
        recurse(USChain(rel, USExactly(n-1, rel)), from) // todo: this is probably quite slow
      }
    }
  }
}
