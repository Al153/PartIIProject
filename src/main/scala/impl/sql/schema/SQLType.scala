package impl.sql.schema

import core.schema._
import impl.sql.SQLColumnName
import impl.sql.tables.SQLTable

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

  def toTypeString(t: SQLType): String = t match { // todo: are these correct?
    case SQLInt => "INT NOT NULL"
    case SQLBool => "BOOLEAN NOT NULL"
    case SQLDouble => "DOUBLE NOT NULL"
    case SQLString => "TEXT"
    case SQLRef => "BIGINT"
    case SQLPrimaryRef => "BIGINT NOT NULL PRIMARY"
    case SQLForeignRef(table) => "BIGINT"
  }
}