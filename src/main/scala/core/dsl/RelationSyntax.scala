package core.dsl

import core.NodeDef
import prototyping.{??, Findable, SchemaObject}

/**
  * Created by Al on 11/10/2017.
  */
object RelationSyntax {
  implicit class RelationalQuerySyntax[A <: NodeDef, B <: NodeDef](left: RelationalQuery[A, B]) {
    def --><--[C <: NodeDef, R](that: R)(implicit f: R => RelationalQuery[C, B], bSchema: SchemaObject[B]): RelationalQuery[A, C]  = left.plus(??(bSchema), f(that).reverse)
    def -->-->[C <: NodeDef, R](that: R)(implicit f: R => RelationalQuery[B, C], bSchema: SchemaObject[B]): RelationalQuery[A, C] = left.plus(??(bSchema), f(that))
    def -->[C <: NodeDef](r: Findable[B]) = new HalfQuery(left, r)

    def |[R](that: R)(implicit f: R => RelationalQuery[A, B]) = left.union(f(that))
    def &[R](that: R)(implicit f: R => RelationalQuery[A, B]) = left.intersection(f(that))


    case class HalfQuery[A <: NodeDef, B <: NodeDef](left: RelationalQuery[A, B], middle: Findable[B]) {
      def -->[C <: NodeDef](that: RelationalQuery[B, C]): RelationalQuery[A, C]  = left.plus(middle,  that)
      def <--[C <: NodeDef](that: RelationalQuery[C, B]): RelationalQuery[A, C]  = left.plus(middle, that.reverse)
    }
  }


}
