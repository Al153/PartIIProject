package impl.sql.tables

import core.backend.common.DBObject
import core.containers.ConstrainedFuture
import core.error.E
import core.intermediate.unsafe.SchemaObjectErased
import core.view.View
import impl.sql.errors.ColumnMismatchException
import impl.sql.types.{Commit, ObjId}
import impl.sql.{ObjectTableName, SQLColumnName, SQLInstance, errors}
import impl.sql.jdbc.Conversions._
import impl.sql.schema.{SQLPrimaryRef, SQLSchema, SQLType}

import scalaz.Scalaz._
import scalaz._

class ObjectTable(
                   override val name: ObjectTableName,
                   instance: SQLInstance,
                   tableSchema: SchemaObjectErased
                 ) extends SQLTable {

  import instance.executionContext

  def getColumnName(i: Int): E \/ SQLColumnName =
    if (i >= 0 &&  i < tableSchema.length)
      SQLColumnName.column(i).right
    else ColumnMismatchException(i, tableSchema.name, tableSchema.schemaComponents).left // todo: Create an error if too large
  
  // search for an appropriate object in the view, if there isn't, insert one to the new commit. return the ObjId
  def insertOrGetObject(
                         dBObject: DBObject,
                         view: View,
                         newCommit: Commit
                       ): ConstrainedFuture[E, ObjId] = ConstrainedFuture.either {
    val temp = "insertOrGetTemp"
    val pairs = createComparisons(dBObject)
    val columnNames = getColumnNames(dBObject).mkString(", ")
    val values = getValues(dBObject).mkString(", ")

    val query = s"""
       |WITH $temp AS (
       |  INSERT INTO $name ($columnNames)
       |  SELECT $values WHERE NOT EXISTS (SELECT 0 FROM $name WHERE $pairs)
       |  RETURNING $id
       |) SELECT * FROM $temp UNION ALL (SELECT ${SQLColumnName.objId} FROM $name WHERE $pairs
     """.stripMargin

    instance.reader.getObj(query)
  }(errors.recoverSQLException)

  override def schema: SQLSchema = SQLSchema(
    Map(
      ObjectTable.objId -> SQLPrimaryRef
    ) ++ SQLType.getRegularSchema(tableSchema.schemaComponents)
  )
}

object ObjectTable {
  val objId: SQLColumnName = SQLColumnName.objId
}