package core.dsl

import core.schema.{??, Findable, SchemaObject}

/**
  * Created by Al on 11/10/2017.
  */
object RelationSyntax {
  implicit class RelationalQuerySyntax[A, B](left: RelationalQuery[A, B])(implicit sa: SchemaObject[A], sb: SchemaObject[B]) {
    def --><--[C](that: RelationalQuery[C, B])(implicit sc: SchemaObject[C]): RelationalQuery[A, C]  = left.plusDistinct(??(sb), that.reverse)
    def -->-->[C](that: RelationalQuery[B, C])(implicit sc: SchemaObject[C]): RelationalQuery[A, C] = left.plus(??(sb), that)
    def -->[C](r: Findable[B]) = HalfQuery(left, r)
    def -->[C](b: B) = HalfQuery(left, sb.findable(b))

    def |(that: RelationalQuery[A, B]): RelationalQuery[A, B] = left.union(that)
    def &(that: RelationalQuery[A, B]): RelationalQuery[A, B] = left.intersection(that)

    case class HalfQuery[A, B](left: RelationalQuery[A, B], middle: Findable[B]) {
      def -->[C](that: RelationalQuery[B, C])(implicit sc: SchemaObject[C]): RelationalQuery[A, C]  = left.plus(middle,  that)
      def <--[C](that: RelationalQuery[C, B])(implicit sc: SchemaObject[C]): RelationalQuery[A, C]  = left.plusDistinct(middle, that.reverse)
    }
  }


}
