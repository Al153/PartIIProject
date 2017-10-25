package core.intermediate

import core.RelationAttributes
import core.intermediate.unsafe._
import schema.{Findable, SchemaObject}

/**
  * Created by Al on 23/10/2017.
  */
sealed abstract class FindPair[A, B](implicit val sa: SchemaObject[A], val sb: SchemaObject[B]) {
  def reverse: FindPair[B, A]
  def getUnsafe: UnsafeFindPair
}
// basic
case class Rel[A, B](r: RelationAttributes[A, B])(implicit sa: SchemaObject[A], sb: SchemaObject[B]) extends FindPair[A, B] {
  override def reverse: FindPair[B, A] = RevRel(r)
  override def getUnsafe = USRel(UnsafeRelationAttributes(r))
}
case class RevRel[A, B](r: RelationAttributes[B, A])(implicit sa: SchemaObject[A], sb: SchemaObject[B]) extends FindPair[A, B] {
 override def reverse: FindPair[B, A] = Rel(r)
  override def getUnsafe = USRevRel(UnsafeRelationAttributes(r))
}
case class And[A, B](left: FindPair[A, B], right: FindPair[A, B])(implicit sa: SchemaObject[A], sb: SchemaObject[B]) extends FindPair[A, B] {
  override def reverse: FindPair[B, A] = And(left.reverse, right.reverse)
  override def getUnsafe = USAnd(left.getUnsafe, right.getUnsafe)
}
case class Or[A, B](left: FindPair[A, B], right: FindPair[A, B])(implicit sa: SchemaObject[A], sb: SchemaObject[B]) extends FindPair[A, B] {
  override def reverse: FindPair[B, A] = Or(left.reverse, right.reverse)
  override def getUnsafe = USOr(left.getUnsafe, right.getUnsafe)
}
case class Chain[A, B, C](left: FindPair[A, B], right: FindPair[B, C])(implicit sa: SchemaObject[A], sb: SchemaObject[B], sc: SchemaObject[C]) extends FindPair[A, C] {
  override def reverse: FindPair[C, A] = Chain(right.reverse, left.reverse)
  override def getUnsafe = USChain(left.getUnsafe, right.getUnsafe)
}
case class Narrow[A, B](rel: FindPair[A, B], pattern: Findable[B])(implicit sa: SchemaObject[A], sb: SchemaObject[B]) extends FindPair[A, B] {
  override def reverse: FindPair[B, A] = Chain(Narrow(Id(), pattern), rel.reverse)
  override def getUnsafe = USNarrow(rel.getUnsafe, UnsafeFindable(pattern))
}
case class Id[A]()(implicit sa: SchemaObject[A]) extends FindPair[A, A] {
  override def reverse: FindPair[A, A] = this
  override def getUnsafe = USId
}

// Repetition
case class Exactly[A](n: Int, rel: FindPair[A, A])(implicit sa: SchemaObject[A]) extends FindPair[A, A] {
  override def reverse: FindPair[A, A] = Exactly(n, rel.reverse)
  override def getUnsafe = USExactly(n, rel.getUnsafe)
}
case class Upto[A]( n: Int, rel: FindPair[A, A])(implicit sa: SchemaObject[A]) extends FindPair[A, A] {
  override def reverse: FindPair[A, A] = Upto(n, rel.reverse)
  override def getUnsafe = USUpto(n, rel.getUnsafe)
}
case class Atleast[A](n: Int, rel: FindPair[A, A])(implicit sa: SchemaObject[A]) extends FindPair[A, A] {
  override def reverse: FindPair[A, A] = Atleast(n, rel.reverse)
  override def getUnsafe = USAtleast(n, rel.getUnsafe)
}
case class Between[A](low: Int, high: Int, rel: FindPair[A, A])(implicit sa: SchemaObject[A]) extends FindPair[A, A] {
  override def reverse: FindPair[A, A] = Between(low, high, rel.reverse)
  override def getUnsafe = USBetween(low, high, rel.getUnsafe)
}



