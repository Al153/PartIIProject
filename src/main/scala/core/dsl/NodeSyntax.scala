package core.dsl

import core.intermediate.From
import schema.SchemaObject


object NodeSyntax {

  implicit class NodeSyntax1[A](u: A)(implicit sa: SchemaObject[A]) {
    def reachableWith[B](r: RelationalQuery[A, B])(implicit sb: SchemaObject[B]): From[A, B] = r.from(u)

  }
}