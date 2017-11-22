package impl.sql.tables

import core.error.E
import core.intermediate.unsafe.UnsafeFindable
import impl.sql.{ObjectTableName, SQLColumnName, SQLDB, SQLInstance}

import scalaz.\/

class ObjectTable(
                   name: ObjectTableName,
                   instance: SQLInstance,
                   prototype: UnsafeFindable
                 ) {
  import ObjectTable._

  def getColumnName(i: Int): E \/ SQLColumnName
}

object ObjectTable {
  val objId = SQLColumnName.objId
}