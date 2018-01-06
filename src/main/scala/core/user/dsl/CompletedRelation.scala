package core.user.dsl

import core.user.schema.SchemaObject

/**
  * Created by Al on 04/10/2017.
  */
case class CompletedRelation[A, B]
(a: A, r: RelationAttributes[A, B], b: B)
(implicit sa: SchemaObject[A], sb: SchemaObject[B]) {

}