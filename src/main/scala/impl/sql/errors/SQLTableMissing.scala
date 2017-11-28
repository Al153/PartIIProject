package impl.sql.errors

import core.schema.TableName

case class SQLTableMissing(n: TableName) extends SQLError