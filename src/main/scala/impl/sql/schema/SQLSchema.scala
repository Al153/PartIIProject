package impl.sql.schema

import core.schema.SchemaDescription
import impl.sql.view.ViewsTable
import impl.sql.{SQLColumnName, SQLTableName}


case class SQLSchema(n: SQLTableName, v: Map[SQLColumnName, SQLType])

sealed trait SQLType
case object SQLString extends SQLType
case object SQLInt extends SQLType
case object SQLBool extends SQLType
case object SQLDouble extends SQLType
case object SQLRef extends SQLType // a reference type

trait Storeable

object SQLSchema {
  def fromSchema(s: SchemaDescription): SQLSchema = ???

  val viewsSchema = SQLSchema(SQLTableName.viewsTable, Map(ViewsTable.viewID -> SQLRef, ViewsTable.commitID -> SQLRef))
}