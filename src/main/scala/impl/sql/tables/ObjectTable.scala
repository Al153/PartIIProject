package impl.sql.tables

import core.backend.common.DBObject
import core.user.schema.SchemaObject
import impl.sql.jdbc.Conversions._
import impl.sql.names.{ObjectTableName, SQLColumnName}
import impl.sql.schema.{SQLPrimaryRef, SQLSchema, SQLType}
import impl.sql.types.ObjId
import impl.sql.{SQLFuture, SQLInstance, _}

/**
  * Table looking up and extracting objects
  * One exists for each [[SchemaObject]] in the owning [[SQLInstance]]'s Schema description
  */
class ObjectTable(
                   override val name: ObjectTableName,
                   override val instance: SQLInstance,
                   tableSchema: SchemaObject[_]
                 ) extends SQLTable {

  import ObjectTable._
  import instance.executionContext

  // Each object table has an auxiliary table to associate each commit with its objects
  val auxTable: AuxObjectTable = new AuxObjectTable(this)


  /**
    * search for an appropriate object in the view, if there isn't,
    * insert one to the new commit. return the ObjId
    */

  def insertOrGetObject(
                         dBObject: DBObject
                       ): SQLFuture[ObjId] = SQLFutureE {
    val temp = "insertOrGetTemp" // name for temp value
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

  /**
    *   Schema: ObjId, and appropriate type for each column.
    *
    */
  override def schema: SQLSchema = SQLSchema(
    Map(
      ObjectTable.objId -> SQLPrimaryRef
    ) ++ SQLType.getObjectSchema(tableSchema.schemaComponents),
    uniqueRelation = true)
}

object ObjectTable {
  // import useful values
  val objId: SQLColumnName = SQLColumnName.objId
}