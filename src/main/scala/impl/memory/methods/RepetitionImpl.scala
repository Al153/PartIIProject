package impl.memory.methods

import core.backend.common.algorithms.FixedPointTraversal
import core.error.E
import impl.memory.{MemoryObject, RelatedPair}
import core.utils._

import scalaz.Scalaz._
import scalaz.\/

trait RepetitionImpl { self: ExecutorMethods with Joins =>
  /**
    *   Find the fixed point of the search step function
    */

  protected def fixedPoint(searchStep: Set[MemoryObject] => E \/ Set[MemoryObject], initial: Set[RelatedPair]): E \/ Set[RelatedPair] =
    FixedPointTraversal.fixedPoint(searchStep, initial)


  protected def upTo(
                      searchStep: Set[MemoryObject] => E \/ Set[MemoryObject],
                      initial: Set[MemoryObject],
                      limit: Int): E \/ Set[RelatedPair] = FixedPointTraversal.upTo(searchStep, initial, limit)

}