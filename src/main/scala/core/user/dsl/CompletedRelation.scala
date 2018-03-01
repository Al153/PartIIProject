package core.user.dsl

import core.user.schema.SchemaObject

/**
  * Created by Al on 04/10/2017.
  * A [[CompletedRelation]] is an insertable relation
  */
case class CompletedRelation[A, B]
(a: A, r: Relation[A, B], b: B)
(implicit sa: SchemaObject[A], sb: SchemaObject[B]) {

}