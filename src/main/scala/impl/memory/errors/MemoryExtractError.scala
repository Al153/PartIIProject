package impl.memory.errors

import core.backend.common.ExtractError

/**
  * Created by Al on 04/01/2018.
  *
  * Wraps an [[ExtractError]]
  */
case class MemoryExtractError(e: ExtractError) extends MemoryError{

}
