package impl.sql.errors

import core.backend.common.MissingRelation

/**
  * Created by Al on 19/12/2017.
  *
  * Occurs when there is a missing relation error when converting a query to unsafe
  */
case class SQLMissingRelation(e: MissingRelation) extends SQLError {

}
