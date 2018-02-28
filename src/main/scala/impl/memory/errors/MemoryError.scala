package impl.memory.errors

import core.backend.common.{ExtractError, MissingRelation}
import core.user.dsl.{E, ViewId}
import core.user.schema.TableName

/**
  * Created by Al on 04/01/2018.
  *
  * Trait hierarchy
  */
sealed trait MemoryError extends E {

}

/**
  * Created by Al on 04/01/2018.
  *
  * Error which catches exceptions
  */
case class CaughtMemoryException(e: Throwable) extends MemoryError {
  override def toString: String = "CAUGHT UNKNOWN MEMORY EXCEPTION: " + e.toString + "\n" + e.getStackTrace.mkString("\n") + e.getCause.toString
}

/**
  * Created by Al on 04/01/2018.
  *
  * Wraps an [[ExtractError]]
  */
case class MemoryExtractError(e: ExtractError) extends MemoryError{

}

/**
  * Created by Al on 04/01/2018.
  *
  * Error for when a relation is missing from the Instance
  */
case class MemoryMissingRelation(mr: MissingRelation) extends MemoryError {

}

/**
  * Created by Al on 04/01/2018.
  *
  * Error for when a [[TableName]] is missing from the instance
  */
case class MemoryMissingTableName(t: TableName) extends MemoryError

/**
  * Created by Al on 22/10/2017.
  *
  * Error when there is a missing view
  */
case class MissingViewError(v: ViewId) extends MemoryError {

}
