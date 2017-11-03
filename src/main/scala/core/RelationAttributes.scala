package core

import core.dsl.RelationalQuery
import core.intermediate.unsafe.ErasedRelationAttributes
import core.intermediate.{FindPair, Rel}
import schema.{RelationName, SchemaDescription, SchemaObject, TableName}


/**
  * Created by Al on 15/10/2017.
  */
abstract class RelationAttributes[A, B](implicit sa: SchemaObject[A], sb: SchemaObject[B]) extends RelationalQuery[A, B]{
  def tableNames: (TableName, TableName) = (sa.tableName, sb.tableName)
  override def tree(implicit sd: SchemaDescription): FindPair[A, B] = Rel(this)
}


