package core.backend.intermediate

import core.backend.common.MissingRelation
import core.backend.intermediate.unsafe.{UnsafeFindPair, _}
import core.user.dsl.{FindPairAble, RelationAttributes}
import core.user.schema.{SchemaDescription, SchemaObject}

import scalaz.Scalaz._
import scalaz.\/

/**
  * Created by Al on 23/10/2017.
  *
  * Sealed trait hierarchy for the internal AST
  *
  * This set of objects is typically used to construct the AST in a type safe manner, which is then converted to
  * is unsafe (type-erased) equivalent for execution
  */
sealed abstract class FindPair[A, B](implicit val sa: SchemaObject[A], val sb: SchemaObject[B]) extends FindPairAble[A, B] {
  /**
    * Reverse the relation
    */
  def reverse: FindPair[B, A]

  /**
    * Erase the type of the object, wrt SchemaDescription
    */
  def getUnsafe(sd: SchemaDescription):  MissingRelation \/ UnsafeFindPair

  /**
    * Reflexively return this
    */

  override def toFindPair: FindPair[A, B] = this
}

/**
  * Search for pairs related by a [[RelationAttributes]]
  */
case class Rel[A, B](r: RelationAttributes[A, B])(implicit sa: SchemaObject[A], sb: SchemaObject[B]) extends FindPair[A, B] {
  override def reverse: FindPair[B, A] = RevRel(r)
  override def getUnsafe(sd: SchemaDescription):  MissingRelation \/ UnsafeFindPair = for {erased <- sd.getRelation(r)} yield USRel(erased)
}

/**
  * Search for pairs related in reverse by a [[RelationAttributes]]
  */
case class RevRel[A, B](r: RelationAttributes[B, A])(implicit sa: SchemaObject[A], sb: SchemaObject[B]) extends FindPair[A, B] {
 override def reverse: FindPair[B, A] = Rel(r)
  override def getUnsafe(sd: SchemaDescription):  MissingRelation \/ UnsafeFindPair = for {erased <- sd.getRelation(r)} yield USRevRel(erased)
}

/**
  * Search for pairs that appear in the result of both subexpressions
  */

case class And[A, B](left: FindPair[A, B], right: FindPair[A, B])(implicit sa: SchemaObject[A], sb: SchemaObject[B]) extends FindPair[A, B] {
  override def reverse: FindPair[B, A] = And(left.reverse, right.reverse)
  override def getUnsafe(sd: SchemaDescription):  MissingRelation \/ UnsafeFindPair = for {
    l <- left.getUnsafe(sd)
    r <- right.getUnsafe(sd)
  } yield USAnd(l, r)
}

/**
  * Search pairs in the result of the left subexpression, such that the right of the pair is in result of the right subexpression
  */
case class AndRight[A, B](left: FindPair[A, B], right: FindSingle[B])(implicit sa: SchemaObject[A], sb: SchemaObject[B]) extends FindPair[A, B] {
  override def reverse: FindPair[B, A] = AndLeft(left.reverse, right)
  override def getUnsafe(sd: SchemaDescription):  MissingRelation \/ UnsafeFindPair = for {
      l <- left.getUnsafe(sd)
      r <- right.getUnsafe(sd)
    } yield USAndRight(l, r)
}

/**
  * Search for pairs of the left sub expression such that the left of the pair is in the result of the right subexpression
  */
case class AndLeft[A, B](
                          left: FindPair[A, B],
                          right: FindSingle[A]
                        )(implicit sa: SchemaObject[A], sb: SchemaObject[B]) extends FindPair[A, B] {

  override def reverse: FindPair[B, A] = AndRight(left.reverse, right)
  override def getUnsafe(sd: SchemaDescription):  MissingRelation \/ UnsafeFindPair = for {
    l <- left.getUnsafe(sd)
    r <- right.getUnsafe(sd)
  } yield USAndLeft(l, r)
}


/**
  * Search for pairs that appear in the result of either subexpression
  */
case class Or[A, B](left: FindPair[A, B], right: FindPair[A, B])(implicit sa: SchemaObject[A], sb: SchemaObject[B]) extends FindPair[A, B] {
  override def reverse: FindPair[B, A] = Or(left.reverse, right.reverse)
  override def getUnsafe(sd: SchemaDescription): MissingRelation \/ UnsafeFindPair = for {
    l <- left.getUnsafe(sd)
    r <- right.getUnsafe(sd)
  } yield USOr(l, r)
}

/**
  * search for pairs (a, c) such that there exists b, (a, b) is in the result of left, (b, c) is in the result of right
  */

case class Chain[A, B, C](
                           left: FindPair[A, B],
                           right: FindPair[B, C]
                         )(implicit sa: SchemaObject[A], sb: SchemaObject[B], sc: SchemaObject[C]) extends FindPair[A, C] {
  override def reverse: FindPair[C, A] = Chain(right.reverse, left.reverse)
  override def getUnsafe(sd: SchemaDescription): MissingRelation \/ UnsafeFindPair = for {
    l <- left.getUnsafe(sd)
    r <- right.getUnsafe(sd)
  } yield USChain(l, r)
}

/**
  * return a set of repeated pairs of the type
  *
  * Chain(a, id) = Chain(id, a) = a
  */

case class FindIdentity[A]()(implicit sa: SchemaObject[A]) extends FindPair[A, A] {
  override def reverse: FindPair[A, A] = this
  override def getUnsafe(sd: SchemaDescription): MissingRelation \/ UnsafeFindPair = USId(sa.name).right[MissingRelation]
}

/**
  * filter the result of the sub expression for those pairs which do not equal each other
  */

case class Distinct[A, B](rel: FindPair[A, B])(implicit sa: SchemaObject[A], sb: SchemaObject[B]) extends FindPair[A, B] {
  override def reverse: FindPair[B, A] = Distinct(rel.reverse)
  override def getUnsafe(sd: SchemaDescription): MissingRelation \/ USDistinct = for {r <- rel.getUnsafe(sd)} yield USDistinct(r)
}

/**
  * Find pairs that are related by n repetitions of the underlying subexpression
  */

case class Exactly[A](n: Int, rel: FindPair[A, A])(implicit sa: SchemaObject[A]) extends FindPair[A, A] {
  override def reverse: FindPair[A, A] = Exactly(n, rel.reverse)
  override def getUnsafe(sd: SchemaDescription): MissingRelation \/ UnsafeFindPair = for {
    r <- rel.getUnsafe(sd)
  } yield USExactly(n, r)
}

/**
  * Find pairs that are related by up to n repetitions of the underlying subexpression
  */

case class Upto[A]( n: Int, rel: FindPair[A, A])(implicit sa: SchemaObject[A]) extends FindPair[A, A] {
  override def reverse: FindPair[A, A] = Upto(n, rel.reverse)
  override def getUnsafe(sd: SchemaDescription): MissingRelation \/ UnsafeFindPair = for {
    r <- rel.getUnsafe(sd)
  } yield USUpto(n, r)
}

/**
  * Find pairs that are related by the transitive closure of the underlying subexpression
  */
case class FixedPoint[A](rel: FindPair[A, A])(implicit sa: SchemaObject[A]) extends FindPair[A, A] {
  override def reverse: FindPair[A, A] = FixedPoint(rel.reverse)
  override def getUnsafe(sd: SchemaDescription): MissingRelation \/ UnsafeFindPair = for {
    r <- rel.getUnsafe(sd)
  } yield USFixedPoint(r)
}



