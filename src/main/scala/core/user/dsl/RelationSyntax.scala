package core.user.dsl

import core.backend.intermediate.{Find, FindSingle, RelationalQuery}
import core.user.schema.{??, Findable, SchemaDescription, SchemaObject}

/**
  * Created by Al on 11/10/2017.
  */
trait RelationSyntax {
  implicit class RelationalQuerySyntax[A, B](left: RelationalQuery[A, B])(implicit sa: SchemaObject[A], sb: SchemaObject[B]) {
    def --><--[C](that: RelationalQuery[C, B])(implicit sc: SchemaObject[C]): RelationalQuery[A, C]  =
      if (left == that) left.plusDistinct(??(sb), that.reverse)
      else left.plus(??(sb), that.reverse)
    def -->-->[C](that: RelationalQuery[B, C])(implicit sc: SchemaObject[C]): RelationalQuery[A, C] = left.plus(??(sb), that)
    def -->[C](r: Findable[B]) = HalfQuery(left, r)
    def -->[C](b: B) = HalfQuery(left, sb.findable(b))

    def -->>(right: FindSingle[B]): RelationalQuery[A, B] = left.rightAnd(right)
    def -->>(right: Findable[B])(implicit sd: SchemaDescription): RelationalQuery[A, B] = left.rightAnd(Find(right))


    def |(that: RelationalQuery[A, B]): RelationalQuery[A, B] = left.union(that)
    def &(that: RelationalQuery[A, B]): RelationalQuery[A, B] = left.intersection(that)

    case class HalfQuery[A, B](left: RelationalQuery[A, B], middle: Findable[B]) {
      def -->[C](that: RelationalQuery[B, C])(implicit sc: SchemaObject[C]): RelationalQuery[A, C]  = left.plus(middle,  that)
      def <--[C](that: RelationalQuery[C, B])(implicit sc: SchemaObject[C]): RelationalQuery[A, C]  = left.plusDistinct(middle, that.reverse)
    }
  }


}
