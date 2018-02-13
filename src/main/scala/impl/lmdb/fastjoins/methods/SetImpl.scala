package impl.lmdb.fastjoins.methods

import core.backend.intermediate.unsafe._
import core.user.schema.SchemaObject
import core.utils.algorithms.{FixedPointTraversal, Joins}
import core.utils.{EitherOps, _}
import impl.lmdb.common.LMDBEither
import impl.lmdb.common.access.{Commit, ObjId}
import impl.lmdb.common.errors.{LMDBError, MissingIndex}
import impl.lmdb.common.tables.impl.ObjectRetrievalTable

import scalaz.Scalaz._

/**
  * Created by Al on 30/12/2017.
  *
  * Implementation for set methods
  */
trait SetImpl { self: Methods =>

  /**
    * Run a type-erased findable and get the result
    * @param ut - query to run
    * @param commits - commits to look at
    * @param objectTable - table to extract from
    * @return
    */
  def readSet[A](
                     ut: UnsafeFindSingle,
                     commits: List[Commit],
                     objectTable: ObjectRetrievalTable
                   )(implicit sa: SchemaObject[A]): LMDBEither[Set[A]] = {
    for {
      // find the object ids of the result
      ids <- getSingles(ut, commits)
      // extract the objects
      res <- objectTable.retrieve(ids)
    } yield res
  }
  /**
    * Run a type-erased findable and get the result
    * @param ut - query to run
    * @param commits - commits to look at
    * @param leftTable - table to extract left of each pair from
    * @param rightTable - table to extract right of each pair from
    * @return
    */
  def readSetPairs[A, B](
                          ut: UnsafeFindPair,
                          commits: List[Commit],
                          leftTable: ObjectRetrievalTable,
                          rightTable: ObjectRetrievalTable
                        )(
    implicit sa: SchemaObject[A],
    sb: SchemaObject[B]
  ): LMDBEither[Set[(A, B)]] = for {
    // starting values (all values in left table)
    initial <- leftTable.lookup(commits)
    // runs the ADT on the intitial values
    pairs <- getPairs(ut, commits, initial)
    // extract the pairs
    res <- extractPairSet[A, B](pairs, leftTable, rightTable)
  } yield res

  /**
    * Speeds up extracting values from a pair of sets by building an index
    * @param in - Pairs to find results for
    * @return All pairs extracted from the tables
    */
  def extractPairSet[A, B](in: Set[(ObjId, ObjId)], aTable: ObjectRetrievalTable, bTable: ObjectRetrievalTable)(
    implicit sa: SchemaObject[A],
    sb: SchemaObject[B]
  ): LMDBEither[Set[(A, B)]] = {
    val leftIds = in.mapProj1
    val rightIds = in.mapProj2

    for {
      // build up an index for left and right hand side - O(root(N)) size index
      // todo delegate and use a single transaction
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
}
