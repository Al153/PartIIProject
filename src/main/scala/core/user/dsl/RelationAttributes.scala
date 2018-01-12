package core.user.dsl

import core.backend.intermediate.{FindPair, Rel, RelationalQuery}
import core.user.schema.{SchemaDescription, SchemaObject, TableName}


/**
  * Created by Al on 15/10/2017.
  *
  * A [[RelationAttributes]] is the basic [[RelationalQuery]] class
  */
abstract class RelationAttributes[A, B](implicit sa: SchemaObject[A], sb: SchemaObject[B])
  extends RelationalQuery[A, B]{
  /**
    * Names of tables used by the query
    */
  def tableNames: (TableName, TableName) = (sa.name, sb.name)

  /**
    * Convert the RelationalAttributes into an ADT
    */
  override def tree(implicit sd: SchemaDescription): FindPair[A, B] = Rel(this)
}


