package core.user.dsl

import core.backend.intermediate._
import core.user.schema.{SchemaDescription, SchemaObject}

import scala.language.implicitConversions

/**
  * Created by Al on 11/10/2017.
  *
  * Syntax for Relations
  */
trait RelationSyntax {
  /**
    * Relation syntax
    */
  implicit class FindPairSyntax[A, B](left: FindPairAble[A, B])(
    implicit sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription
  ) {
    /**
      * Chain with another relation, which has been reversed
      */
    def --><--[C](right: FindPairAble[C, B])(implicit sc: SchemaObject[C]): FindPair[A, C] =
      if (left == right) Distinct(Chain(left.toFindPair, right.toFindPair.reverse)(sa, sb, sc))(sa, sc)
      else Chain(left.toFindPair, right.toFindPair.reverse)(sa, sb, sc)

    /**
      * Chain with another relation, provided the types match
      */
    def -->-->[C](right: FindPairAble[B, C])(implicit sc: SchemaObject[C]): FindPair[A, C] =
      Chain(left.toFindPair, right.toFindPair)(sa, sb, sc)

    /**
      * Initiate chaining but with a findable in the middle
      */
    def -->(r: FindSingleAble[B]): FindPairHalfQuery[A, B] = FindPairHalfQuery(left.toFindPair, r)(sa, sb)

    /**
      * Initiate chaining but specifying the element to join on
      */
    def -->(b: B): FindPairHalfQuery[A, B] = FindPairHalfQuery(left.toFindPair, sb.findable(b))(sa, sb)


    /**
      * Right filter the relation by a FindSingle
      */

    def -->>(right: FindSingleAble[B]): FindPair[A, B] =
      AndRight(left.toFindPair, right.toFindSingle)(sa, sb)

    /**
      * Union a relation
      */
    def |(that: FindPairAble[A, B]): FindPair[A, B] = Or(left.toFindPair, that.toFindPair)(sa, sb)

    /**
      * Intersect a relation
      */
    def &(that: FindPairAble[A, B]): FindPair[A, B] = And(left.toFindPair, that.toFindPair)(sa, sb)

  }

  case class FindPairHalfQuery[A, B](left: FindPairAble[A, B], middle: FindSingleAble[B])(implicit sa: SchemaObject[A], sb: SchemaObject[B]) {
    /**
      * Chain the second query in a forwards direction
      */
    def -->[C](right: FindPairAble[B, C])(implicit sc: SchemaObject[C]): FindPair[A, C]  =
      Distinct(Chain(AndRight(left.toFindPair, middle.toFindSingle), right.toFindPair))

    /**
      * Chain the second query in a backwards direction
      */
    def <--[C](right: FindPairAble[C, B])(implicit sc: SchemaObject[C]): FindPair[A, C]  =
      Distinct(Chain(AndRight(left.toFindPair, middle.toFindSingle), right.toFindPair.reverse))
  }

  implicit class SymmetricQueryOps[A](u: FindPairAble[A, A])(
    implicit sa: SchemaObject[A], sd: SchemaDescription
  ) {
    def *(n: Int): FindPair[A, A] = Exactly(n, u.toFindPair)

    def * (r: Repetition): FindPair[A, A] = r match {
      case BetweenRange(lo, hi) => Between(lo, hi, u.toFindPair)(sa)
      case UptoRange(n) => Upto(n, u.toFindPair)(sa)
      case AtleastRange(n) => Atleast(n, u.toFindPair)(sa)
    }

    def ** : FindPair[A, A] = Atleast(0, u.toFindPair)(sa)

    def ++ : FindPair[A, A] = Atleast(1, u.toFindPair)(sa)

    def ? : FindPair[A, A] = Upto(1, u.toFindPair)(sa)
  }


}
