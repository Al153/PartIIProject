package core.user.dsl

import core.backend.intermediate.{FindPair, Rel, RelationalQuery}
import core.user.schema.{SchemaDescription, SchemaObject, TableName}


/**
  * Created by Al on 15/10/2017.
  */
abstract class RelationAttributes[A, B](implicit sa: SchemaObject[A], sb: SchemaObject[B]) extends RelationalQuery[A, B]{
  def tableNames: (TableName, TableName) = (sa.tableName, sb.tableName)
  override def tree(implicit sd: SchemaDescription): FindPair[A, B] = Rel(this)
}


