package core

import core.dsl.RelationalQuery
import core.intermediate.{GetRelation, IntermediateTree}
import schema.SchemaRelation


/**
  * Created by Al on 17/09/2017.
  */
trait RelationAttributes[A <: NodeDef, B <: NodeDef] extends RelationalQuery[A, B]{
  override def tree(a: IntermediateTree[A]): IntermediateTree[(A, B)] = GetRelation(a, this)
}


