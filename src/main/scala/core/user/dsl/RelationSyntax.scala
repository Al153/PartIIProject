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
      if (left == right) Distinct(Chain(left.toFindPair, right.toFindPair.reverse)(sa, sb, sc, sd))(sa, sc, sd)
      else Chain(left.toFindPair, right.toFindPair.reverse)(sa, sb, sc, sd)

    /**
      * Chain with another relation, provided the types match
      */
    def -->-->[C](right: FindPairAble[B, C])(implicit sc: SchemaObject[C]): FindPair[A, C] =
      Chain(left.toFindPair, right.toFindPair)(sa, sb, sc, sd)

    /**
      * Initiate chaining but with a findable in the middle
      */
    def -->(r: FindSingleAble[B]): FindPairHalfQuery[A, B] = FindPairHalfQuery(left.toFindPair, r)(sa, sb, sd)

    /**
      * Initiate chaining but specifying the element to join on
      */
    def -->(b: B): FindPairHalfQuery[A, B] = FindPairHalfQuery(left.toFindPair, sb.findable(b))(sa, sb, sd)


    /**
      * Right filter the relation by a FindSingle
      */

    def -->>(right: FindSingleAble[B]): FindPair[A, B] =
      AndRight(left.toFindPair, right.toFindSingle)(sa, sb, sd)

    /**
      * Union a relation
      */
    def |(that: FindPairAble[A, B]): FindPair[A, B] = Or(left.toFindPair, that.toFindPair)(sa, sb, sd)

    /**
      * Intersect a relation
      */
    def &(that: FindPairAble[A, B]): FindPair[A, B] = And(left.toFindPair, that.toFindPair)(sa, sb, sd)

  }

  case class FindPairHalfQuery[A, B](left: FindPairAble[A, B], middle: FindSingleAble[B])(implicit sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription) {
    /**
      * Chain the second query in a forwards direction
      */
    def -->[C](right: FindPairAble[B, C])(implicit sc: SchemaObject[C]): FindPair[A, C]  =
      Distinct(Chain(AndRight(left.toFindPair, middle.toFindSingle), right.toFindPair)(sa, sb, sc, sd))(sa, sc, sd)

    /**
      * Chain the second query in a backwards direction
      */
    def <--[C](right: FindPairAble[C, B])(implicit sc: SchemaObject[C]): FindPair[A, C]  =
      Distinct(Chain(AndRight(left.toFindPair, middle.toFindSingle), right.toFindPair.reverse)(sa, sb, sc, sd))(sa, sc, sd)
  }

  implicit class SymmetricQueryOps[A](u: FindPairAble[A, A])(
    implicit sa: SchemaObject[A], sd: SchemaDescription
  ) {
    def *(n: Int): FindPair[A, A] = Exactly(n, u.toFindPair)(sa, sd)

    def * (r: Repetition): FindPair[A, A] = r match {
      case BetweenRange(lo, hi) => Between(lo, hi, u.toFindPair)(sa, sd)
      case UptoRange(n) => Upto(n, u.toFindPair)(sa, sd)
      case AtleastRange(n) => Atleast(n, u.toFindPair)(sa, sd)
    }

    def ** : FindPair[A, A] = Atleast(0, u.toFindPair)(sa, sd)

    def ++ : FindPair[A, A] = Atleast(1, u.toFindPair)(sa, sd)

    def ? : FindPair[A, A] = Upto(1, u.toFindPair)(sa, sd)
  }


}
