package core.backend.intermediate

import core.backend.common.MissingRelation
import core.user.dsl.{E, RelationAttributes}
import core.backend.intermediate.unsafe._
import core.user.schema.{Findable, SchemaDescription, SchemaObject}

import scalaz.Scalaz._
import scalaz._

/**
  * Created by Al on 23/10/2017.
  */
sealed abstract class FindPair[A, B](implicit val sa: SchemaObject[A], val sb: SchemaObject[B], sd: SchemaDescription) {
  def reverse: FindPair[B, A]
  def getUnsafe:  MissingRelation \/ UnsafeFindPair
}
// Search for pairs related by a relation
case class Rel[A, B](r: RelationAttributes[A, B])(implicit sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription) extends FindPair[A, B] {
  override def reverse: FindPair[B, A] = RevRel(r)
  override def getUnsafe:  MissingRelation \/ UnsafeFindPair = for {erased <- sd.getRelation(r)} yield USRel(erased)
}

// Search for pairs related by a relation backwards
case class RevRel[A, B](r: RelationAttributes[B, A])(implicit sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription) extends FindPair[A, B] {
 override def reverse: FindPair[B, A] = Rel(r)
  override def getUnsafe:  MissingRelation \/ UnsafeFindPair = for {erased <- sd.getRelation(r)} yield USRevRel(erased)
}

// Search for pairs that appear in the result of both subexpressions
case class And[A, B](left: FindPair[A, B], right: FindPair[A, B])(implicit sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription) extends FindPair[A, B] {
  override def reverse: FindPair[B, A] = And(left.reverse, right.reverse)
  override def getUnsafe:  MissingRelation \/ UnsafeFindPair = for {
    l <- left.getUnsafe
    r <- right.getUnsafe
  } yield USAnd(l, r)
}

// Search pairs in the result of the left subexpression, such that the right of the pair is in result of the right subexpression
case class AndSingle[A, B](left: FindPair[A, B], right: FindSingle[B])(implicit sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription) extends FindPair[A, B] {
  override def reverse: FindPair[B, A] = Chain(AndSingle(new FindIdentity(), right), left.reverse)
  override def getUnsafe:  MissingRelation \/ UnsafeFindPair = for {
      l <- left.getUnsafe
      r <- right.getUnsafe
    } yield USAndSingle(l, r)
}

// Search for pairs that appear in the result of either subexpression
case class Or[A, B](left: FindPair[A, B], right: FindPair[A, B])(implicit sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription) extends FindPair[A, B] {
  override def reverse: FindPair[B, A] = Or(left.reverse, right.reverse)
  override def getUnsafe: MissingRelation \/ UnsafeFindPair = for {
    l <- left.getUnsafe
    r <- right.getUnsafe
  } yield USOr(l, r)
}

// search for pairs (a, c) such that there exists b, (a, b) is in the result of left, (b, c) is in the result of right
case class Chain[A, B, C](left: FindPair[A, B], right: FindPair[B, C])(implicit sa: SchemaObject[A], sb: SchemaObject[B], sc: SchemaObject[C], sd: SchemaDescription) extends FindPair[A, C] {
  override def reverse: FindPair[C, A] = Chain(right.reverse, left.reverse)
  override def getUnsafe: MissingRelation \/ UnsafeFindPair = for {
    l <- left.getUnsafe
    r <- right.getUnsafe
  } yield USChain(l, r)
}

// Search pairs in the result of the left subexpression, such that the right of the pair matches the pattern
case class Narrow[A, B](rel: FindPair[A, B], pattern: Findable[B])(implicit sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription) extends FindPair[A, B] {
  override def reverse: FindPair[B, A] = Chain(Narrow(new FindIdentity(), pattern), rel.reverse)
  override def getUnsafe: MissingRelation \/ UnsafeFindPair = for {
    r <- rel.getUnsafe
  } yield USNarrow(r, pattern.getUnsafe)
}

// return a set of repeated pairs of the type
case class FindIdentity[A]()(implicit sa: SchemaObject[A], sd: SchemaDescription) extends FindPair[A, A] {
  override def reverse: FindPair[A, A] = this
  override def getUnsafe: MissingRelation \/ UnsafeFindPair = USId(sa.tableName).right[MissingRelation]
}

// filter the result of the sub expression for those pairs which do not equal each other
case class Distinct[A, B](rel: FindPair[A, B])(implicit sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription) extends FindPair[A, B] {
  override def reverse: FindPair[B, A] = Distinct(rel.reverse)
  override def getUnsafe: MissingRelation \/ USDistinct = for {r <- rel.getUnsafe} yield USDistinct(r)
}

// Find pairs that are related by n repetitions of the underlying subexpression
case class Exactly[A](n: Int, rel: FindPair[A, A])(implicit sa: SchemaObject[A], sd: SchemaDescription) extends FindPair[A, A] {
  override def reverse: FindPair[A, A] = Exactly(n, rel.reverse)
  override def getUnsafe: MissingRelation \/ UnsafeFindPair = for {
    r <- rel.getUnsafe
  } yield USExactly(n, r)
}

// Find pairs that are related by up to n repetitions of the underlying subexpression
case class Upto[A]( n: Int, rel: FindPair[A, A])(implicit sa: SchemaObject[A], sd: SchemaDescription) extends FindPair[A, A] {
  override def reverse: FindPair[A, A] = Upto(n, rel.reverse)
  override def getUnsafe: MissingRelation \/ UnsafeFindPair = for {
    r <- rel.getUnsafe
  } yield USUpto(n, r)
}

// Find pairs that are related by at least n repetitions of the underlying subexpression
case class Atleast[A](n: Int, rel: FindPair[A, A])(implicit sa: SchemaObject[A], sd: SchemaDescription) extends FindPair[A, A] {
  override def reverse: FindPair[A, A] = Atleast(n, rel.reverse)
  override def getUnsafe: MissingRelation \/ UnsafeFindPair = for {
    r <- rel.getUnsafe
  } yield USAtleast(n, r)
}

// Find pairs that are related by a number of repetitions (between low and high) of the underlying subexpression
case class Between[A](low: Int, high: Int, rel: FindPair[A, A])(implicit sa: SchemaObject[A], sd: SchemaDescription) extends FindPair[A, A] {
  override def reverse: FindPair[A, A] = Between(low, high, rel.reverse)
  override def getUnsafe: MissingRelation \/ UnsafeFindPair = for {
    r <- rel.getUnsafe
  } yield USBetween(low, high, r)
}



