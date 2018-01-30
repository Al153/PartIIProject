package impl.lmdbfast.methods

import core.utils.algorithms.{FixedPointTraversal, Joins}
import core.backend.intermediate.unsafe._
import core.user.schema.SchemaObject
import core.utils.{EitherOps, _}
import impl.lmdbfast.LMDBEither
import impl.lmdbfast.access.{Commit, ObjId}
import impl.lmdbfast.errors.{LMDBError, MissingIndex}
import impl.lmdbfast.tables.impl.ObjectRetrievalTable

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
      ids <- findSingleSet(ut, commits)
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
    pairs <- findPairSet(ut, commits, initial)
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
      // todo: merge indices if sa = sb?
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

  /**
    * Interpret a [[UnsafeFindSingle]] AST to get all appropriate ObjIds
    * Fairly straight forward
    * @param ut - The AST to interpret
    * @param commits - Commits to search
    * @return
    */
  def findSingleSet(ut: UnsafeFindSingle, commits: List[Commit]): LMDBEither[Set[ObjId]] = {
    def recurse(t: UnsafeFindSingle) = findSingleSet(t, commits)
    ut match {
      case USFind(pattern) => instance.lookupPattern(pattern, commits)

      case USFrom(start, rel) => for {
        left <- recurse(start)
        res <- findPairSet(rel, commits, left)
      } yield res.mapProj2

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
    * Interpret a [[UnsafeFindPair]] AST to get all appropriate ObjIds
    * Fairly straight forward recursive implementation
    * @param ut - The AST to interpret
    * @param commits - Commits to search
    * @param from - the left hand object Ids we want to include. We use
    *               this because we want to limit the number of objects
    *               we search as much as possible
    * @return
    */
  protected def findPairSet(
                             ut: UnsafeFindPair,
                             commits: List[Commit],
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
      } yield leftRes.filter{case (_, b) => rightRes.contains(b)}

      case USAndLeft(l, r) => for {
        leftRes <- recurse(l, from)
        rightRes <- findSingleSet(r, commits)
      } yield leftRes.filter{case (a, _) => rightRes.contains(a)}


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

      case USFixedPoint(rel) =>
        // find a fixed point
        val stepFunction: Set[ObjId] => LMDBEither[Set[ObjId]] = left => recurse(rel, left).map(_.mapProj2)
        FixedPointTraversal.fixedPoint(stepFunction, from.mapPair)

      case USExactly(n, rel) =>
        for {
          fsCache <- precomputeFindSingles(rel, commits)
          stepFunction: (ObjId => LMDBEither[Set[ObjId]]) = {left: ObjId => findFrom(left, rel, commits, fsCache)}
          r <- FixedPointTraversal.exactly(stepFunction, from, n)
        }  yield  r

    }
  }
}
