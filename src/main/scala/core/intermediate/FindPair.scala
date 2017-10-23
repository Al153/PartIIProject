package core.intermediate

import core.RelationAttributes
import schema.{Findable, SchemaObject}

/**
  * Created by Al on 23/10/2017.
  */
sealed abstract class FindPair[A, B](implicit val sa: SchemaObject[A], val sb: SchemaObject[B]) {
  def reverse: FindPair[B, A]
}
// basic
case class Rel[A, B](r: RelationAttributes[A, B])(implicit sa: SchemaObject[A], sb: SchemaObject[B]) extends FindPair[A, B] {
  override def reverse: FindPair[B, A] = RevRel(r)
}
case class RevRel[A, B](r: RelationAttributes[B, A])(implicit sa: SchemaObject[A], sb: SchemaObject[B]) extends FindPair[A, B] {
 override def reverse: FindPair[B, A] = Rel(r)
}
case class And[A, B](left: FindPair[A, B], right: FindPair[A, B])(implicit sa: SchemaObject[A], sb: SchemaObject[B]) extends FindPair[A, B] {
  override def reverse: FindPair[B, A] = And(left.reverse, right.reverse)
}
case class Or[A, B](left: FindPair[A, B], right: FindPair[A, B])(implicit sa: SchemaObject[A], sb: SchemaObject[B]) extends FindPair[A, B] {
  override def reverse: FindPair[B, A] = Or(left.reverse, right.reverse)
}
case class Chain[A, B, C](left: FindPair[A, B], right: FindPair[B, C])(implicit sa: SchemaObject[A], sb: SchemaObject[B], sc: SchemaObject[C]) extends FindPair[A, C] {
  override def reverse: FindPair[C, A] = Chain(right.reverse, left.reverse)
}
case class Narrow[A, B](rel: FindPair[A, B], pattern: Findable[B])(implicit sa: SchemaObject[A], sb: SchemaObject[B]) extends FindPair[A, B] {
  override def reverse: FindPair[B, A] = Chain(Narrow(Id(), pattern), rel.reverse)
}
case class Id[A]()(implicit sa: SchemaObject[A]) extends FindPair[A, A] {
  override def reverse: FindPair[A, A] = this
}

// Repetition
case class Exactly[A](n: Int, rel: FindPair[A, A])(implicit sa: SchemaObject[A]) extends FindPair[A, A] {
  override def reverse: FindPair[A, A] = Exactly(n, rel.reverse)
}
case class Upto[A]( n: Int, rel: FindPair[A, A])(implicit sa: SchemaObject[A]) extends FindPair[A, A] {
  override def reverse: FindPair[A, A] = Upto(n, rel.reverse)
}
case class AtLeast[A](n: Int, rel: FindPair[A, A])(implicit sa: SchemaObject[A]) extends FindPair[A, A] {
  override def reverse: FindPair[A, A] = AtLeast(n, rel.reverse)
}
case class Between[A](low: Int, high: Int, rel: FindPair[A, A])(implicit sa: SchemaObject[A]) extends FindPair[A, A] {
  override def reverse: FindPair[A, A] = Between(low, high, rel.reverse)
}



