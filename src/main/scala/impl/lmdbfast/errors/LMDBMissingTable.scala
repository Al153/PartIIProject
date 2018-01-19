package impl.lmdbfast.errors

import core.user.schema.TableName

/**
  * Created by Al on 29/12/2017.
  *
  * Thrown when an unknown TableName is looked up
  */
case class LMDBMissingTable(table: TableName) extends LMDBError {

}
