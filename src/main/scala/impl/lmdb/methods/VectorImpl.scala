package impl.lmdb.methods

import core.backend.common.algorithms.{FixedPointTraversal, Joins}
import core.backend.intermediate.unsafe._
import core.user.schema.SchemaObject
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
  *
  * Implementation for multiset (Vector) methods
  */
trait VectorImpl { self: Methods =>

  /**
    * Run a type-erased findable and get the result
    * @param ut - query to run
    * @param commits - commits to look at
    * @param objectTable - table to extract from
    * @return
    */
  def readVector[A](
                       ut: UnsafeFindSingle,
                       commits: Set[Commit],
                       objectTable: ObjectRetrievalTable
                     )(implicit extractor: SchemaObject[A]): LMDBEither[Vector[A]] =
    for {
    // find the object ids of the result
      ids <- findSingleVector(ut, commits)
      // extract the objects
      res <- objectTable.retrieve(ids)
    } yield res

  /**
    * Run a type-erased findable and get the result
    * @param ut - query to run
    * @param commits - commits to look at
    * @param leftTable - table to extract left of each pair from
    * @param rightTable - table to extract right of each pair from
    * @return
    */

  def readVectorPairs[A, B](
                           ut: UnsafeFindPair,
                           commits: Set[Commit],
                           leftTable: ObjectRetrievalTable,
                           rightTable: ObjectRetrievalTable
                           )(
    implicit sa: SchemaObject[A],
    sb: SchemaObject[B]
  ): LMDBEither[Vector[(A, B)]] = for {
  // starting values (all values in left table)
    initial <- leftTable.lookupVector(commits)
    // runs the ADT on the intitial values
    pairs <- findPairVector(ut, commits, initial)
    // extract the pairs
    res <- extractPairVector[A, B](pairs, leftTable, rightTable)
  } yield res

  /**
    * Speeds up extracting values from a pair of sets by building an index
    * @param in - Pairs to find results for
    * @return All pairs extracted from the tables
    */
  def extractPairVector[A, B](
                               in: Vector[(ObjId, ObjId)],
                               aTable: ObjectRetrievalTable,
                               bTable: ObjectRetrievalTable
                             )(
    implicit sa: SchemaObject[A],
    sb: SchemaObject[B]
  ): LMDBEither[Vector[(A, B)]] = {
    val leftIds = in.mapProj1.toSet
    val rightIds = in.mapProj2.toSet
    for {
    // build up an index for left and right hand side - O(root(N)) size index
    // todo: merge indices if sa = sb?



      leftIndex <- EitherOps.sequence(leftIds.map {
        id => aTable.retrieve[A](id).map(id -> _)
      }).toMapE

      rightIndex <- EitherOps.sequence(rightIds.map {
        id => bTable.retrieve[B](id).map(id -> _)
      }).toMapE


      // read from index
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
  }

  /**
    * Interpret a [[UnsafeFindSingle]] AST to get all appropriate ObjIds
    * Fairly straight forward
    * @param ut - The AST to interpret
    * @param commits - Commits to search
    * @return
    */
  def findSingleVector(ut: UnsafeFindSingle, commits: Set[Commit]): LMDBEither[Vector[ObjId]] = {
    def recurse(t: UnsafeFindSingle) = findSingleVector(t, commits)

    ut match {
      case USFind(pattern) =>
        instance.lookupPattern(pattern, commits).map(_.toVector)
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

  /**
    * Interpret a [[UnsafeFindPair]] AST to get all appropriate ObjIds
    * Fairly straight forward recursive implementation
    * @param ut - The AST to interpret
    * @param commits - Commits to search
    * @param from - the left hand object Ids we want to include. We use
    *               this because we want to limit the number of objects
    *               we search as much as possible
    * @return
    */

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


      case USAndRight(l, r) => for {
        leftRes <- recurse(l, from)
        rightRes <- findSingleSet(r, commits)
      } yield leftRes.filter{case (a, b) => rightRes.contains(b)}

      case USAndLeft(l, r) => for {
        leftRes <- recurse(l, from)
        rightRes <- findSingleSet(r, commits)
      } yield leftRes.filter{case (a, b) => rightRes.contains(a)}


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
        val stepFunction: Set[ObjId] => LMDBEither[Set[ObjId]] = left => findPairSet(rel, commits, left).map(_.mapProj2)
        for {
          rres <- FixedPointTraversal.upTo[LMDBError, ObjId](stepFunction, from.toSet, n)
        } yield rres.toVector

      case USFixedPoint(rel) =>
        // find a fixed point
        val stepFunction: Set[ObjId] => LMDBEither[Set[ObjId]] = left => findPairSet(rel, commits, left).map(_.mapProj2)
        for {
          res <- FixedPointTraversal.fixedPoint(stepFunction, from.toSet.mapPair)
        } yield res.toVector

      case USExactly(n, rel) => if (n <= 0) {
        from.map(x => (x, x)).right
      } else {
        recurse(USChain(rel, USExactly(n-1, rel)), from) // todo: this is probably quite slow
      }
    }
  }
}
