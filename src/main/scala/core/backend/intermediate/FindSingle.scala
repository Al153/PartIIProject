package core.backend.intermediate

import core.backend.common.MissingRelation
import core.backend.intermediate.unsafe._
import core.user.dsl.FindSingleAble
import core.user.schema.{Findable, SchemaDescription, SchemaObject}

import scalaz.Scalaz._
import scalaz._

/**
  * Created by Al on 23/10/2017.
  *
  *  Typesafe construction of AST of queries to lookup single objects
  */

sealed abstract class FindSingle[A](implicit val sa: SchemaObject[A]) extends FindSingleAble[A] {
  /**
    * Convert to unsafe
    */
  def getUnsafe(sd: SchemaDescription): MissingRelation \/ UnsafeFindSingle

  /**
    * Get a FindSingle
    */

  final override def toFindSingle: FindSingle[A] = this
}

/**
  * Lookup a findable
  */
case class Find[A](pattern: Findable[A])(implicit sa: SchemaObject[A]) extends FindSingle[A] {
  override def getUnsafe(sd: SchemaDescription): MissingRelation \/ UnsafeFindSingle = USFind(pattern.getUnsafe).right
}

/**
  * Narrow down a findSingle query
  */
case class NarrowS[A](start: FindSingle[A], pattern: Findable[A])(implicit sa: SchemaObject[A]) extends FindSingle[A] {
  override def getUnsafe(sd: SchemaDescription): MissingRelation \/ UnsafeFindSingle = for {
    usStart <- start.getUnsafe(sd)
  } yield USNarrowS(usStart, pattern.getUnsafe)
}

/**
  * Find objects b: B, such that there exists a: A in the result of start that (a, b) is in the result of rels
  */
case class From[A, B](start: FindSingle[A], rel: FindPair[A, B])(implicit sa: SchemaObject[A], sb: SchemaObject[B]) extends FindSingle[B] {
  override def getUnsafe(sd: SchemaDescription): MissingRelation \/ UnsafeFindSingle = for {
    unsafeRel <- rel.getUnsafe(sd)
    unsafeStart <- start.getUnsafe(sd)
  } yield USFrom(unsafeStart, unsafeRel)
}


