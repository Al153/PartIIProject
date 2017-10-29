package core.intermediate

import core.intermediate.unsafe._
import schema.{Findable, SchemaObject}

/**
  * Created by Al on 23/10/2017.
  */

sealed abstract class FindSingle[A](implicit val sa: SchemaObject[A]) {
  def getUnsafe: UnsafeFindSingle
}
case class Find[A](pattern: Findable[A])(implicit sa: SchemaObject[A]) extends FindSingle[A] {
  override def getUnsafe: UnsafeFindSingle = USFind(pattern.getUnsafe)
}
case class NarrowS[A](start: FindSingle[A], pattern: Findable[A])(implicit sa: SchemaObject[A]) extends FindSingle[A] {
  override def getUnsafe: UnsafeFindSingle = USNarrowS(start.getUnsafe, pattern.getUnsafe)
}
case class From[A, B](start: FindSingle[A], rel: FindPair[A, B])(implicit sa: SchemaObject[A], sb: SchemaObject[B]) extends FindSingle[B] {
  override def getUnsafe: UnsafeFindSingle = USFrom(start.getUnsafe, rel.getUnsafe)
}
