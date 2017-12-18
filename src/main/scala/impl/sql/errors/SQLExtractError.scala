package impl.sql.errors

import core.backend.common.ExtractError

/**
  * Created by Al on 18/12/2017.
  */
case class SQLExtractError(e: ExtractError) extends SQLError {
  override def toString: String = e.toString
}
