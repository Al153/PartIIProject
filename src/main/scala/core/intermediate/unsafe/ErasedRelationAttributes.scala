package core.intermediate.unsafe

import core.relations.RelationAttributes
import core.schema.{RelationName, TableName}

/**
  * Created by Al on 25/10/2017.
  */
case class ErasedRelationAttributes(name: RelationName, from: TableName, to: TableName )

object ErasedRelationAttributes {
  def apply[A, B](name: RelationName, rab: RelationAttributes[A, B]): ErasedRelationAttributes = new ErasedRelationAttributes(name, rab.tableNames._1, rab.tableNames._2)
}