package core.concrete.relations

import core.RelationAttributes
import schema.SchemaObject

/**
  * Created by Al on 04/10/2017.
  */
case class CompletedRelation[A, B, R](a: A, r: R, b: B)(implicit f: R => RelationAttributes[A, B], sa: SchemaObject[A], sb: SchemaObject[B]) {

}