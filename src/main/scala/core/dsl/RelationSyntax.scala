package core.dsl

import schema.{??, Findable, SchemaObject}

/**
  * Created by Al on 11/10/2017.
  */
object RelationSyntax {
  implicit class RelationalQuerySyntax[A, B](left: RelationalQuery[A, B])(implicit sa: SchemaObject[A], sb: SchemaObject[B]) {
    def --><--[C, R](that: R)(implicit f: R => RelationalQuery[C, B], sc: SchemaObject[C]): RelationalQuery[A, C]  = left.plus(??(sb), f(that).reverse)
    def -->-->[C, R](that: R)(implicit f: R => RelationalQuery[B, C], sc: SchemaObject[C]): RelationalQuery[A, C] = left.plus(??(sb), f(that))
    def -->[C](r: Findable[B]) = HalfQuery(left, r)

    def |[R](that: R)(implicit f: R => RelationalQuery[A, B]) = left.union(f(that))
    def &[R](that: R)(implicit f: R => RelationalQuery[A, B]) = left.intersection(f(that))


    case class HalfQuery[A, B](left: RelationalQuery[A, B], middle: Findable[B]) {
      def -->[C](that: RelationalQuery[B, C])(implicit sc: SchemaObject[C]): RelationalQuery[A, C]  = left.plus(middle,  that)
      def <--[C](that: RelationalQuery[C, B])(implicit sc: SchemaObject[C]): RelationalQuery[A, C]  = left.plus(middle, that.reverse)
    }
  }


}
