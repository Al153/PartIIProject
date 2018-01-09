package core.user.dsl

import core.backend.intermediate.{Find, FindSingle, RelationalQuery}
import core.user.schema.{Findable, SchemaDescription, SchemaObject}

/**
  * Created by Al on 08/01/2018.
  *
  * Syntax for findables
  */

trait FindableSyntax {
  implicit class FindableSyntax1[A](u: Findable[A])(implicit sa: SchemaObject[A]) {
    /**
      * Chain this findable into a Relational query - ie a UnaryQuery for objects linked by the relational query from
      * those that match this findable
      */
    def >>[B](r: RelationalQuery[A, B])(implicit sb: SchemaObject[B], sd: SchemaDescription): FindSingle[B] = r.from(u)
    /**
      * Left-filter a relation by this findable
      */
    def -->>[B](r: RelationalQuery[A, B])(implicit sb: SchemaObject[B], sd: SchemaDescription): RelationalQuery[A, B] = r.leftAnd(Find(u))
  }
}
