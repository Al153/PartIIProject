package core.intermediate

import core.RelationAttributes
import core.error.E
import core.intermediate.unsafe._
import schema.{Findable, SchemaDescription, SchemaObject}

import scalaz._, Scalaz._

/**
  * Created by Al on 23/10/2017.
  */
sealed abstract class FindPair[A, B](implicit val sa: SchemaObject[A], val sb: SchemaObject[B], sd: SchemaDescription) {
  def reverse: FindPair[B, A]
  def getUnsafe: E \/ UnsafeFindPair
}
// basic
case class Rel[A, B](r: RelationAttributes[A, B])(implicit sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription) extends FindPair[A, B] {
  override def reverse: FindPair[B, A] = RevRel(r)
  override def getUnsafe: \/[E, USRel] = for {erased <- sd.getRelation(r)} yield USRel(erased)
}
case class RevRel[A, B](r: RelationAttributes[B, A])(implicit sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription) extends FindPair[A, B] {
 override def reverse: FindPair[B, A] = Rel(r)
  override def getUnsafe: \/[E, USRevRel] = for {erased <- sd.getRelation(r)} yield USRevRel(erased)
}
case class And[A, B](left: FindPair[A, B], right: FindPair[A, B])(implicit sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription) extends FindPair[A, B] {
  override def reverse: FindPair[B, A] = And(left.reverse, right.reverse)
  override def getUnsafe: \/[E, USAnd] = for {
    l <- left.getUnsafe
    r <- right.getUnsafe
  } yield USAnd(l, r)
}

case class AndSingle[A, B](left: FindPair[A, B], right: FindSingle[B])(implicit sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription) extends FindPair[A, B] {
  override def reverse: FindPair[B, A] = Chain(AndSingle(Id(), right), left.reverse)
  override def getUnsafe: E \/ UnsafeFindPair = for {
      l <- left.getUnsafe
    } yield USAndSingle(l, right.getUnsafe)
}

case class Or[A, B](left: FindPair[A, B], right: FindPair[A, B])(implicit sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription) extends FindPair[A, B] {
  override def reverse: FindPair[B, A] = Or(left.reverse, right.reverse)
  override def getUnsafe = for {
    l <- left.getUnsafe
    r <- right.getUnsafe
  } yield USOr(l, r)
}
case class Chain[A, B, C](left: FindPair[A, B], right: FindPair[B, C])(implicit sa: SchemaObject[A], sb: SchemaObject[B], sc: SchemaObject[C], sd: SchemaDescription) extends FindPair[A, C] {
  override def reverse: FindPair[C, A] = Chain(right.reverse, left.reverse)
  override def getUnsafe = for {
    l <- left.getUnsafe
    r <- right.getUnsafe
  } yield USChain(l, r)
}
case class Narrow[A, B](rel: FindPair[A, B], pattern: Findable[B])(implicit sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription) extends FindPair[A, B] {
  override def reverse: FindPair[B, A] = Chain(Narrow(Id(), pattern), rel.reverse)
  override def getUnsafe = for {
    r <- rel.getUnsafe
  } yield USNarrow(r, pattern.getUnsafe)
}
case class Id[A]()(implicit sa: SchemaObject[A]) extends FindPair[A, A] {
  override def reverse: FindPair[A, A] = this
  override def getUnsafe: E \/ UnsafeFindPair = USId.right[E]
}

// Repetition
case class Exactly[A](n: Int, rel: FindPair[A, A])(implicit sa: SchemaObject[A], sd: SchemaDescription) extends FindPair[A, A] {
  override def reverse: FindPair[A, A] = Exactly(n, rel.reverse)
  override def getUnsafe = for {
    r <- rel.getUnsafe
  } yield USExactly(n, r)
}
case class Upto[A]( n: Int, rel: FindPair[A, A])(implicit sa: SchemaObject[A], sd: SchemaDescription) extends FindPair[A, A] {
  override def reverse: FindPair[A, A] = Upto(n, rel.reverse)
  override def getUnsafe = for {
    r <- rel.getUnsafe
  } yield USUpto(n, r)
}
case class Atleast[A](n: Int, rel: FindPair[A, A])(implicit sa: SchemaObject[A], sd: SchemaDescription) extends FindPair[A, A] {
  override def reverse: FindPair[A, A] = Atleast(n, rel.reverse)
  override def getUnsafe = for {
    r <- rel.getUnsafe
  } yield USAtleast(n, r)
}
case class Between[A](low: Int, high: Int, rel: FindPair[A, A])(implicit sa: SchemaObject[A], sd: SchemaDescription) extends FindPair[A, A] {
  override def reverse: FindPair[A, A] = Between(low, high, rel.reverse)
  override def getUnsafe = for {
    r <- rel.getUnsafe
  } yield USBetween(low, high, r)
}



