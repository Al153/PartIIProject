package impl.sql.errors

import core.error.E
import core.schema.TableName

case class SQLTableMissing(n: TableName) extends E