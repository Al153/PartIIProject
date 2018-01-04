package impl.memory.methods

import core.backend.common.algorithms.FixedPointTraversal
import impl.memory.{MemoryEither, MemoryObject, RelatedPair}

trait RepetitionImpl { self: ExecutorMethods with Joins =>
  /**
    *   Find the fixed point of the search step function
    */

  protected def fixedPoint(
                            searchStep: Set[MemoryObject] => MemoryEither[Set[MemoryObject]],
                            initial: Set[RelatedPair]
                          ): MemoryEither[Set[RelatedPair]] =
    FixedPointTraversal.fixedPoint(searchStep, initial)


  protected def upTo(
                      searchStep: Set[MemoryObject] => MemoryEither[Set[MemoryObject]],
                      initial: Set[MemoryObject],
                      limit: Int):  MemoryEither[Set[RelatedPair]] = FixedPointTraversal.upTo(searchStep, initial, limit)

}