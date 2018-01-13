package core.user.dsl

import core.backend.intermediate._
import core.user.schema.SchemaObject

import scala.language.implicitConversions

/**
  * Created by Al on 08/01/2018.
  *
  * Syntax for findables
  */

trait FindableSyntax {
  implicit class FindableSyntax1[A](u: FindSingleAble[A])(implicit sa: SchemaObject[A]) {
    /**
      * Chain this node into a Relational query - ie a UnaryQuery for objects linked by the relational query from
      * those that match this  node
      */
    def >>[B](r: FindPairAble[A, B])(implicit sb: SchemaObject[B]): From[A, B] =
      From(u.toFindSingle, r.toFindPair)

    /**
      * Left-filter a relation by this findable
      */
    def -->>[B, That](r: FindPairAble[A, B])(implicit sb: SchemaObject[B]): FindPair[A, B] =
      AndLeft(r.toFindPair, u.toFindSingle)
  }

  implicit def toFindSingleAble[A](a: A)(implicit sa: SchemaObject[A]): FindSingleAble[A] = new FindSingleAble[A] {
    override def toFindSingle: FindSingle[A] = sa.findable(a).toFindSingle
  }

  implicit class NodeSyntax[A](u: A)(implicit sa: SchemaObject[A]) {
    /**
      * Chain this node into a Relational query - ie a UnaryQuery for objects linked by the relational query from
      * those that match this  node
      */
    def >>[B](r: FindPairAble[A, B])(implicit sb: SchemaObject[B]): From[A, B] =
      From(u.toFindSingle, r.toFindPair)

    /**
      * Left-filter a relation by this findable
      */
    def -->>[B, That](r: FindPairAble[A, B])(implicit sb: SchemaObject[B]): FindPair[A, B] =
      AndLeft(r.toFindPair, u.toFindSingle)
  }
}
