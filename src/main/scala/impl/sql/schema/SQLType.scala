package impl.sql.schema

import core.schema._
import core.utils._
import impl.sql.errors.{SQLSchemaTypeCheckError, SQLSchemaUnexpectedType}
import impl.sql.tables.SQLTable
import impl.sql.{SQLColumnName, SQLEither}

import scalaz.Scalaz._
import scalaz._

sealed trait SQLType
case object SQLString extends SQLType
case object SQLInt extends SQLType
case object SQLBool extends SQLType
case object SQLDouble extends SQLType
case class SQLForeignRef(table: SQLTable) extends SQLType // a reference in another table
case object SQLPrimaryRef extends SQLType // unique reference
case object SQLRef extends SQLType // generic reference

object SQLType {
  val MetaData = List(SQLColumnName.objId -> SQLPrimaryRef, SQLColumnName.commitId -> SQLRef)
  def getRegularSchema(vc: Vector[SchemaComponent]): Map[SQLColumnName, SQLType] =
    vc.zipWithIndex.map{case (c, i) => get(i, c)}.toMap

  def get(vc: Vector[SchemaComponent]): Map[SQLColumnName, SQLType] =
    getRegularSchema(vc) ++ MetaData

  def get(index: Int, component: SchemaComponent): (SQLColumnName, SQLType) =
    SQLColumnName.column(index) -> (component match {
      case IntCell =>  SQLInt
      case StringCell => SQLString
      case BoolCell  => SQLBool
      case DoubleCell => SQLDouble
    })

  // gets one of several strings
  def fromString(s: String): SQLEither[String] = {
    val possible = Set("int", "bool", "real", "text", "bigint")
    if (s.toLowerCase in possible) s.right
    else SQLSchemaUnexpectedType(s).left
  }

  def toTypeString(t: SQLType): String = t match { // todo: are these correct?
    case SQLInt => "INT NOT NULL"
    case SQLBool => "BOOLEAN NOT NULL"
    case SQLDouble => "REAL NOT NULL"
    case SQLString => "TEXT NOT NULL"
    case SQLRef => "BIGINT TEXT NOT NULL"
    case SQLPrimaryRef => "BIGSERIAL PRIMARY KEY"
    case SQLForeignRef(table) => "BIGINT"
  }

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