package impl.memory.errors

import core.backend.common.MissingRelation

/**
  * Created by Al on 04/01/2018.
  *
  * Error for when a relation is missing from the Instance
  */
case class MemoryMissingRelation(mr: MissingRelation) extends MemoryError {

}
