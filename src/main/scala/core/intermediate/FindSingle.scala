package core.intermediate

import schema.{Findable, SchemaObject}

/**
  * Created by Al on 23/10/2017.
  */

sealed abstract class FindSingle[A](implicit val sa: SchemaObject[A])
case class Find[A](pattern: Findable[A])(implicit sa: SchemaObject[A]) extends FindSingle[A]
case class NarrowS[A](start: FindSingle[A], pattern: Findable[A])(implicit sa: SchemaObject[A]) extends FindSingle[A]
case class From[A, B](start: FindSingle[A], rel: FindPair[A, B])(implicit sa: SchemaObject[A], sb: SchemaObject[B]) extends FindSingle[B]
