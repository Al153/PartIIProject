package impl.sql.errors

import core.backend.common.MissingRelation

/**
  * Created by Al on 19/12/2017.
  */
case class SQLMissingRelation(e: MissingRelation) extends SQLError {

}
