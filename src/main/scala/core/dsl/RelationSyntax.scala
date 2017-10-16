package core.dsl

import core.NodeDef
import schema.Pattern.{?, Pattern}

/**
  * Created by Al on 11/10/2017.
  */
object RelationSyntax {
  implicit class RelationalQuerySyntax[A <: NodeDef, B <: NodeDef](left: RelationalQuery[A, B]) {
    def --><--[C <: NodeDef, R](that: R)(implicit f: R => RelationalQuery[C, B]): RelationalQuery[A, C]  = left.plus(?[B], that.reverse)
    def -->-->[C <: NodeDef, R](that: R)(implicit f: R => RelationalQuery[B, C]): RelationalQuery[A, C] = left.plus(?[B], that)
    def -->[C <: NodeDef](r: Pattern[B]) = new HalfQuery(left, r)

    def |[R](that: R)(implicit f: R => RelationalQuery[A, B]) = left.union(that)
    def &[R](that: R)(implicit f: R => RelationalQuery[A, B]) = left.intersection(that)


    private class HalfQuery[A <: NodeDef, B <: NodeDef, C <: NodeDef](left: RelationalQuery[A, B], middle: Pattern[B]) {
      def -->[R](that: R)(implicit f: R => RelationalQuery[C, B]): RelationalQuery[A, C]  = left.plus(middle,  that.reverse)
      def <--[R](that: R)(implicit f: R => RelationalQuery[B, C]): RelationalQuery[A, C]  = left.plus(middle, that)
    }
  }


}
