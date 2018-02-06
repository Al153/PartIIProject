package impl.memory.methods

import core.utils.algorithms.FixedPointTraversal
import impl.memory.{MemoryEither, MemoryObject, RelatedPair}

/**
  * Helper methods for dealing with queries with repetition. Just defer to generic equivalent functions
  */
trait RepetitionImpl { self: ExecutorMethods with Joins =>
  /**
    *   Find the fixed point of the search step function
    */

  protected def fixedPoint(
                            searchStep: Set[MemoryObject] => MemoryEither[Set[MemoryObject]],
                            initial: Set[MemoryObject]
                          ): MemoryEither[Set[RelatedPair]] =
    FixedPointTraversal.fixedPoint(searchStep, initial)

  /**
    * Find pairs linked by up to limit repetitions
    */

  protected def upTo(
                      searchStep: MemoryObject => MemoryEither[Set[MemoryObject]],
                      initial: Set[MemoryObject],
                      limit: Int):  MemoryEither[Set[RelatedPair]] = FixedPointTraversal.upTo(searchStep, initial, limit)

}