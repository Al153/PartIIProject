package core.concrete.relations

import core.{NodeDef, RelationAttributes}

/**
  * Created by Al on 04/10/2017.
  */
case class CompletedRelation[A <: NodeDef, B <: NodeDef, R](a: A, r: R, b: B)(implicit f: R => RelationAttributes[A, B]) {

}