package impl.sql.schema

import core.backend.intermediate._
import core.user.schema._
import core.utils._
import impl.sql.errors.{SQLSchemaTypeCheckError, SQLSchemaUnexpectedType}
import impl.sql.tables.SQLTable
import impl.sql.SQLEither
import impl.sql.names.SQLColumnName

import scalaz.Scalaz._
import scalaz._

/**
  * Sealed trait hierarchy for defining the schema of a table
  */

// todo: separate case objects for ViewId, ColumnId
sealed trait SQLType
case object SQLString extends SQLType
case object SQLInt extends SQLType
case object SQLBool extends SQLType
case object SQLDouble extends SQLType
case class SQLForeignRef(table: SQLTable) extends SQLType // a reference in another table
case object SQLPrimaryRef extends SQLType // unique reference
case object SQLRef extends SQLType // generic reference

object SQLType {
  /**
    * Converts an interface-side Schema informaion (ie that from a [[SchemaObject]] into sql side
    * (ie can be written into a PostgreSQL database
    */
  def getObjectSchema(vc: Vector[SchemaComponent]): Map[SQLColumnName, SQLType] =
    vc.zipWithIndex.map{case (c, i) => get(i, c)}.toMap

  /**
    * Lookup the schemaComponent and give its SQL equivalent, plus column name
    */
  private def get(index: Int, component: SchemaComponent): (SQLColumnName, SQLType) =
    SQLColumnName.column(index) -> (component match {
      case IntCell =>  SQLInt
      case StringCell => SQLString
      case BoolCell  => SQLBool
      case DoubleCell => SQLDouble
    })

  /**
    * Validate that a string is a possible type
    * This is called on values extracted from the SQL DB
    */

  def checkString(s: String): SQLEither[String] = {
    val possible = Set("int", "bool", "real", "text", "bigint")
    if (s.toLowerCase in possible) s.toLowerCase.right
    else SQLSchemaUnexpectedType(s).left
  }

  /**
    * Renders an SQL type to the SQL schema strign it represents
    * @param t
    * @return
    */
  def toTypeString(t: SQLType): String = t match {
    case SQLInt => "INT NOT NULL"
    case SQLBool => "BOOLEAN NOT NULL"
    case SQLDouble => "DOUBLE PRECISION NOT NULL"
    case SQLString => "TEXT NOT NULL"
    case SQLRef => "BIGINT TEXT NOT NULL"
    case SQLPrimaryRef => "BIGSERIAL PRIMARY KEY"
    case SQLForeignRef(table) => s"BIGINT REFERENCES ${table.name}"
  }

  /**
    * Check that an extracted type string and expected type match
    */
  def validateType(t: SQLType, toTest: String): SQLEither[Unit] = {
    val expected = t match {
      case SQLInt => "int"
      case SQLBool => "bool"
      case SQLDouble => "real"
      case SQLString => "text"
      case SQLRef => "bigint"
      case SQLPrimaryRef => "bigint"
      case SQLForeignRef(_) => "bigint"
    }

    if (expected == toTest.toLowerCase) ().right
    else SQLSchemaTypeCheckError(t, toTest).left
  }

}