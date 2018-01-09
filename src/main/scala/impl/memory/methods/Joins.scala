package impl.memory.methods

import core.backend.common.algorithms
import impl.memory.RelatedPair

/**
  * In-memory joins
  */

trait Joins {
  /**
    * Use the generic JoinVector implementation
    */
  protected def join(leftRes: Vector[RelatedPair], rightRes: Vector[RelatedPair]): Vector[RelatedPair] =
    algorithms.Joins.joinVector(leftRes, rightRes)

  /**
    * Use the generic joinSet implementation
    */

  protected def joinSet(leftRes: Set[RelatedPair], rightRes: Set[RelatedPair]): Set[RelatedPair] =
    algorithms.Joins.joinSet(leftRes, rightRes)
}