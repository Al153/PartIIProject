package impl.lmdb.errors

import core.user.schema.TableName

/**
  * Created by Al on 29/12/2017.
  */
case class LMDBMissingTable(table: TableName) extends LMDBError {

}
