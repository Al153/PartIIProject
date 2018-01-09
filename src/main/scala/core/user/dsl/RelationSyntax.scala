package core.user.dsl

import core.backend.intermediate.{Find, FindSingle, RelationalQuery}
import core.user.schema.{Findable, SchemaDescription, SchemaObject}

/**
  * Created by Al on 11/10/2017.
  *
  * Syntax for Relations
  */
trait RelationSyntax {

  /**
    * Relation syntax
    */
  implicit class RelationalQuerySyntax[A, B](left: RelationalQuery[A, B])(
    implicit sa: SchemaObject[A], sb: SchemaObject[B]
  ) {
    /**
      * Chain with another relation, which has been reversed
      */
    def --><--[C](that: RelationalQuery[C, B])(implicit sc: SchemaObject[C]): RelationalQuery[A, C]  =
      if (left == that) left.plusDistinct(sb.any, that.reverse)
      else left.plus(sb.any, that.reverse)

    /**
      * Chain with another relation, provided the types match
      */
    def -->-->[C](that: RelationalQuery[B, C])(implicit sc: SchemaObject[C]): RelationalQuery[A, C] =
      left.plus(sb.any, that)

    /**
      * Initiate chaining but with a findable in the middle
      */
    def -->[C](r: Findable[B]) = HalfQuery(left, r)

    /**
      * Initiate chaining but specifying the element to join on
      */
    def -->[C](b: B) = HalfQuery(left, sb.findable(b))


    /**
      * Right filter the relation by a FindSingle
      */

    def -->>(right: FindSingle[B]): RelationalQuery[A, B] = left.rightAnd(right)

    /**
      * Right filter the relation by a Findable
      */

    def -->>(right: Findable[B])(implicit sd: SchemaDescription): RelationalQuery[A, B] = left.rightAnd(Find(right))

    /**
      * Union a relation
      */
    def |(that: RelationalQuery[A, B]): RelationalQuery[A, B] = left.union(that)

    /**
      * Intersect a relation
      */
    def &(that: RelationalQuery[A, B]): RelationalQuery[A, B] = left.intersection(that)
  }

  /**
    * A helper class for joining queries on a findable
    */
  case class HalfQuery[A, B](left: RelationalQuery[A, B], middle: Findable[B]) {
    /**
      * Chain the second query in a forwards direction
      */
    def -->[C](that: RelationalQuery[B, C])(implicit sc: SchemaObject[C]): RelationalQuery[A, C]  = left.plus(middle,  that)

    /**
      * Chain the second query in a backwards direction
      */
    def <--[C](that: RelationalQuery[C, B])(implicit sc: SchemaObject[C]): RelationalQuery[A, C]  = left.plusDistinct(middle, that.reverse)
  }


}
