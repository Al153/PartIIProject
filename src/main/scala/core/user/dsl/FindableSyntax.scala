package core.user.dsl

import core.backend.intermediate.{Find, FindSingle, RelationalQuery}
import core.user.schema.{Findable, SchemaDescription, SchemaObject}

/**
  * Created by Al on 08/01/2018.
  */

trait FindableSyntax {
  implicit class FindableSyntax1[A](u: Findable[A])(implicit sa: SchemaObject[A]) {
    def >>[B](r: RelationalQuery[A, B])(implicit sb: SchemaObject[B], sd: SchemaDescription): FindSingle[B] = r.from(u)
    def -->>[B](r: RelationalQuery[A, B])(implicit sb: SchemaObject[B], sd: SchemaDescription): RelationalQuery[A, B] = r.leftAnd(Find(u))
  }
}
