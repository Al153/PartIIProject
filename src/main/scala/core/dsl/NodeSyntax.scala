package core.dsl

import core.intermediate.From
import core.schema.{SchemaDescription, SchemaObject}


object NodeSyntax {

  implicit class NodeSyntax1[A](u: A)(implicit sa: SchemaObject[A]) {
    def reachableWith[B](r: RelationalQuery[A, B])(implicit sb: SchemaObject[B], sd: SchemaDescription): From[A, B] = r.from(u)
    def >>[B](r: RelationalQuery[A, B])(implicit sb: SchemaObject[B], sd: SchemaDescription): From[A, B] = reachableWith(r)
  }
}