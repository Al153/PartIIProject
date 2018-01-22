package impl.lmdbfast.methods

import core.backend.intermediate.unsafe._
import core.utils._
import impl.lmdbfast.LMDBEither
import impl.lmdbfast.access.{Commit, ObjId}

trait Optimisations { self: Methods =>

  /**
    * Precompute FindSingles in a query so they may be cached
    */
  def getFindSingles(t: UnsafeFindPair): Set[UnsafeFindSingle] =
    t match {

      case USAndLeft(a, b) => getFindSingles(a) + b
      case USAndRight(a, b) => getFindSingles(a) + b

      case USAnd(a, b) => getFindSingles(a) ++ getFindSingles(b)
      case USChain(a, b) => getFindSingles(a) ++ getFindSingles(b)
      case USOr(a, b) => getFindSingles(a) ++ getFindSingles(b)

      case USUpto(_, rel) => getFindSingles(rel)
      case USDistinct(a) => getFindSingles(a)
      case USFixedPoint(rel) => getFindSingles(rel)
      case USExactly(_, rel) => getFindSingles(rel)

      case USId(_) => Set()
      case USRel(_) => Set()
      case USRevRel(_) => Set()
    }

  /**
    * Precompute a cache of FindSingles
    * @param t - query to search
    * @param commits - commis to execute against
    * @return
    */
  def precomputeFindSingles(t: UnsafeFindPair, commits: List[Commit]): LMDBEither[Map[UnsafeFindSingle, Set[ObjId]]] =
    EitherOps.sequence(getFindSingles(t).map {
      fs =>
        for {
          res <- findSingleSet(fs, commits)
        } yield fs -> res
    }).toMapE
}