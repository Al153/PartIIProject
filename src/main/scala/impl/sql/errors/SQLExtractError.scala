package impl.sql.errors

import core.backend.common.ExtractError

/**
  * Wraps an extract error when extracting values from the database
  */
case class SQLExtractError(e: ExtractError) extends SQLError {
  override def toString: String = e.toString
}
