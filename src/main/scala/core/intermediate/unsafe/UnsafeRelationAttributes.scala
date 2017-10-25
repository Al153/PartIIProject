package core.intermediate.unsafe

import core.RelationAttributes
import schema.{RelationName, TableName}

/**
  * Created by Al on 25/10/2017.
  */
case class UnsafeRelationAttributes(name: RelationName, from: TableName, to: TableName )

object UnsafeRelationAttributes {
  def apply[A, B](rab: RelationAttributes[A, B]): UnsafeRelationAttributes = new UnsafeRelationAttributes(rab.name, rab.tableNames._1, rab.tableNames._2)
}