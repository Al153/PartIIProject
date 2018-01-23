package impl.lmdbfast.methods

import core.backend.intermediate.unsafe._
import core.utils._
import core.utils.algorithms.SimpleFixedPointTraversal
import impl.lmdbfast.LMDBEither
import impl.lmdbfast.access.{Commit, ObjId}
import impl.lmdbfast.errors.{LMDBError, MissingCachedQuery}

import scalaz.Scalaz._
import scalaz._

/**
  * Implementation of "right Only" chained parameters
  */
trait PathFindingImpl { self: Methods =>
  protected def findFrom(
                          from: ObjId,
                          ut: UnsafeFindPair,
                          commits: List[Commit],
                          fsCache: Map[UnsafeFindSingle, Set[ObjId]]
                        ): LMDBEither[Set[ObjId]] = {
    def recurse(ut: UnsafeFindPair, from: ObjId): LMDBEither[Set[ObjId]] = findFrom(from, ut, commits, fsCache)

    ut match {
      case USAnd(l, r)  => for {
        a <- recurse(l, from)
        b <- recurse(r, from)
      } yield a intersect b

      case USAndLeft(l, r) => for {
        rightRes <- fsCache.getOrError(r, MissingCachedQuery(r))
        res <-
          // only need to check if appears in RHS
          if (rightRes.contains(from)) recurse(l, from)
          else LMDBEither(Set[ObjId]())
      } yield res

      case USAndRight(l, r) => for {
        res <- recurse(l, from)

        rightRes <- fsCache.getOrError(r, MissingCachedQuery(r))
      } yield res.intersect(rightRes)


      case USOr(l, r) => for {
        leftRes <- recurse(l, from)
        rightRes <- recurse(r, from)
      } yield leftRes.union(rightRes)

      case USChain(l, r) => for {
        lres <- recurse(l, from)
        results <- (for {
          middle <- lres
        } yield recurse(r, middle)).flattenE
      } yield results

      case USDistinct(r) => for {
        rres <- recurse(r, from)
      } yield rres - from


      case USId(_) => Set(from).right

      case USRel(rel) =>
        instance.controlTables.relations.followRelation(from, commits, rel.name)



      case USRevRel(rel) =>
        instance.controlTables.reverseRelations.followRelation(from, commits, rel.name)


      case USUpto(n, rel) =>
        val stepFunction: ObjId => LMDBEither[Set[ObjId]] = left => recurse(rel, left)

        SimpleFixedPointTraversal.upTo[LMDBError, ObjId](stepFunction, from, n)

      case USFixedPoint(rel) =>
        // find a fixed point
        val stepFunction: ObjId => LMDBEither[Set[ObjId]] = left => recurse(rel, left)
        SimpleFixedPointTraversal.fixedPoint(stepFunction, from)

      case USExactly(n, rel) => if (n <= 0) {
        Set(from).right
      } else {
        recurse(USChain(rel, USExactly(n-1, rel)), from) // todo: this is probably quite slow, use exponential chaining/caching?
      }
    }
  }

}