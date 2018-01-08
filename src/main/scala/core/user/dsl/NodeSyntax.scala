package core.user.dsl

import core.backend.intermediate.{Find, FindSingle, From, RelationalQuery}
import core.user.schema.{Findable, SchemaDescription, SchemaObject}


trait NodeSyntax {

  implicit class NodeSyntax1[A](u: A)(implicit sa: SchemaObject[A]) {
    def reachableWith[B](r: RelationalQuery[A, B])(implicit sb: SchemaObject[B], sd: SchemaDescription): From[A, B] = r.from(u)
    def >>[B](r: RelationalQuery[A, B])(implicit sb: SchemaObject[B], sd: SchemaDescription): From[A, B] = reachableWith(r)
  }
}

