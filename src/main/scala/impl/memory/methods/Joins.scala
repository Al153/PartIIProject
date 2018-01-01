package impl.memory.methods

import core.backend.common.algorithms
import core.backend.common.algorithms.Joins
import impl.memory.{MemoryObject, RelatedPair}

/**
  * In-memory joins
  */

trait Joins {
  protected def join(leftRes: Vector[RelatedPair], rightRes: Vector[RelatedPair]): Vector[RelatedPair] =
    Joins.joinVector(leftRes, rightRes)

  protected def joinSet(leftRes: Set[RelatedPair], rightRes: Set[RelatedPair]): Set[RelatedPair] =
    algorithms.Joins.joinSet(leftRes, rightRes)
}