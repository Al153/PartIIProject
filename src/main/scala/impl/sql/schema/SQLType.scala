package impl.sql.schema

import core.schema._
import impl.sql.SQLColumnName

sealed trait SQLType
case object SQLString extends SQLType
case object SQLInt extends SQLType
case object SQLBool extends SQLType
case object SQLDouble extends SQLType
case object SQLForeignRef extends SQLType // a reference in another table
case object SQLPrimaryRef extends SQLType // unique reference
case object SQLRef extends SQLType // generic reference

object SQLType {
  val MetaData = List(SQLColumnName.id -> SQLPrimaryRef, SQLColumnName.commitId -> SQLRef)
  def getRegularSchema (vc: Vector[SchemaComponent]): Map[SQLColumnName, SQLType] =
    vc.zipWithIndex.map{case (c, i) => get(i, c)}.toMap

  def get(vc: Vector[SchemaComponent]): Map[SQLColumnName, SQLType] =
    getRegularSchema(vc) ++ MetaData

  def get(index: Int, component: SchemaComponent): (SQLColumnName, SQLType) = component match {
    case IntCell => SQLColumnName(index.toString) -> SQLInt
    case StringCell => SQLColumnName(index.toString) ->  SQLString
    case BoolCell  => SQLColumnName(index.toString) ->  SQLBool
    case DoubleCell => SQLColumnName(index.toString) ->  SQLDouble
  }

  def toTypeString(t: SQLType): String = t match { // todo: are these correct?
    case SQLInt => "int NOT NULL"
    case SQLBool => "boolean NOT NULL"
    case SQLDouble => "double NOT NULL"
    case SQLString => "varchar(max)"
    case SQLRef => "bigint"
    case SQLPrimaryRef => "bigint NOT NULL PRIMARY"
    case SQLForeignRef => "bigint"
  }
}