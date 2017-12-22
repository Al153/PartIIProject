package core.intermediate

import core.backend.common.MissingRelation
import core.intermediate.unsafe._
import core.schema.{Findable, SchemaDescription, SchemaObject}

import scalaz.Scalaz._
import scalaz._

/**
  * Created by Al on 23/10/2017.
  */

sealed abstract class FindSingle[A](implicit val sa: SchemaObject[A], sd: SchemaDescription) {
  def getUnsafe: MissingRelation \/ UnsafeFindSingle
}
case class Find[A](pattern: Findable[A])(implicit sa: SchemaObject[A], sd: SchemaDescription) extends FindSingle[A] {
  override def getUnsafe: MissingRelation \/ UnsafeFindSingle = USFind(pattern.getUnsafe).right
}
case class NarrowS[A](start: FindSingle[A], pattern: Findable[A])(implicit sa: SchemaObject[A], sd: SchemaDescription) extends FindSingle[A] {
  override def getUnsafe: MissingRelation \/ UnsafeFindSingle = for {
    usStart <- start.getUnsafe
  } yield USNarrowS(usStart, pattern.getUnsafe)
}
case class From[A, B](start: FindSingle[A], rel: FindPair[A, B])(implicit sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription) extends FindSingle[B] {
  override def getUnsafe: MissingRelation \/ UnsafeFindSingle = for {
    unsafeRel <- rel.getUnsafe
    unsafeStart <- start.getUnsafe
  } yield USFrom(unsafeStart, unsafeRel)
}


