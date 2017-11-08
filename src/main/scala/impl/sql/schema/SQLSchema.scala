package impl.sql.schema

import core.intermediate.unsafe.{ErasedRelationAttributes, SchemaObjectErased}
import core.schema._
import impl.sql.view.ViewsTable
import impl.sql.{SQLColumnName, SQLTableName}



case class SQLSchema(n: SQLTableName, v: Map[SQLColumnName, SQLType]) {
  def createTableString = s"""
  |create table $n (
  | ${v.map{case (name, t) => name.toString + " " + SQLType.toTypeString(t)}.mkString(",\n|")}
  |)
""".stripMargin
}

object SQLSchema {
  def apply(s: SchemaObjectErased): SQLSchema = new SQLSchema(SQLTableName(s.name), SQLType.get(s.schemaComponents))
  def fromSchemaObject(s: SchemaDescription): Map[TableName, SQLSchema] =
    s.erasedObjects.map(o => o.name -> SQLSchema(o)).toMap

  def ofRelation(r: ErasedRelationAttributes) =
    SQLSchema(
      SQLTableName(r.name),
      Map(
        SQLColumnName.leftId -> SQLForeignRef,
        SQLColumnName.commitId -> SQLForeignRef,
        SQLColumnName.rightId -> SQLForeignRef
      )
    )

  val viewsSchema = SQLSchema(SQLTableName.viewsTable, Map(ViewsTable.viewID -> SQLRef, ViewsTable.commitID -> SQLForeignRef))

  def createSQLTableEntry(sd: SchemaDescription): String = ???
}