package impl.sql.tables

import core.backend.common.DBObject
import core.error.E
import core.intermediate.unsafe.SchemaObjectErased
import impl.sql.errors.ColumnMismatchException
import impl.sql.jdbc.Conversions._
import impl.sql.names.{ObjectTableName, SQLColumnName}
import impl.sql.schema.{SQLPrimaryRef, SQLSchema, SQLType}
import impl.sql.types.ObjId
import impl.sql.{SQLFuture, SQLInstance, _}

import scalaz.Scalaz._
import scalaz._

class ObjectTable(
                   override val name: ObjectTableName,
                   override val instance: SQLInstance,
                   tableSchema: SchemaObjectErased
                 ) extends SQLTable {

  import ObjectTable._
  import instance.executionContext

  val auxTable: AuxObjectTable = new AuxObjectTable(this)

  def getColumnName(i: Int): E \/ SQLColumnName =
    if (i >= 0 &&  i < tableSchema.length)
      SQLColumnName.column(i).right
    else ColumnMismatchException(i, tableSchema.name, tableSchema.schemaComponents).left // todo: Create an error if too large
  
  // search for an appropriate object in the view, if there isn't, insert one to the new commit. return the ObjId
  def insertOrGetObject(
                         dBObject: DBObject
                       ): SQLFuture[ObjId] = SQLFutureE {
    val temp = "insertOrGetTemp"
    val pairs = createComparisons(dBObject)
    val columnNames = getColumnNames(dBObject).mkString(", ")
    val values = getValues(dBObject).mkString(", ")

    val query = s"""
       |WITH $temp AS (
       |  INSERT INTO $name ($columnNames)
       |  SELECT $values WHERE NOT EXISTS (SELECT 0 FROM $name WHERE $pairs)
       |  RETURNING $objId
       |) SELECT * FROM $temp UNION ALL (SELECT $objId FROM $name WHERE $pairs)""".stripMargin

    instance.reader.getObj(query)
  }

  override def schema: SQLSchema = SQLSchema(
    Map(
      ObjectTable.objId -> SQLPrimaryRef
    ) ++ SQLType.getRegularSchema(tableSchema.schemaComponents),
    uniqueRelation = true)
}

object ObjectTable {
  val objId: SQLColumnName = SQLColumnName.objId
}