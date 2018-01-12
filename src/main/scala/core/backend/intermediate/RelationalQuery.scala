package core.backend.intermediate

import core.user.dsl.{AtleastRange, BetweenRange, Repetition, UptoRange}
import core.backend.intermediate
import core.user.schema.{Findable, SchemaDescription, SchemaObject}
import core.user.dsl._

import scalaz._

/**
  * Created by Al on 12/10/2017.
  */

// todo: can this be factored out and replaced with syntax
abstract class RelationalQuery[A, B](implicit val sa: SchemaObject[A], val sb: SchemaObject[B]) {
  def tree(implicit sd: SchemaDescription): FindPair[A, B]

  def from(a: Findable[A])(implicit sd: SchemaDescription): FindSingle[B] = {
    From(Find(a), tree)
  }

  def from(a: A)(implicit sd: SchemaDescription) = From(Find(sa.findable(a)), tree)



  // chain together two queries in sequence
  def plus[C](middle: Findable[B], right: RelationalQuery[B, C])(implicit sc: SchemaObject[C]): RelationalQuery[A, C] =
    if (middle.isEmpty) {
      val left = this
      implicit val outerSb = sb
      new RelationalQuery[A, C] {
        override def tree(implicit sd: SchemaDescription): FindPair[A, C] =
          Chain(left.tree, right.tree)(sa, outerSb, sc, sd)
      }
    } else {
      val left = this
      implicit val outerSb = sb
      new RelationalQuery[A, C] {
        override def tree(implicit sd: SchemaDescription): FindPair[A, C] =
          Chain(AndRight(left.tree, Find(middle)), right.tree)(sa, outerSb, sc, sd)
      }
    }

  def plusDistinct[C](middle: Findable[B], right: RelationalQuery[B, C])(implicit sc: SchemaObject[C]): RelationalQuery[A, C] =
    if (middle.isEmpty) {
      val left = this
      implicit val outerSb = sb
      new RelationalQuery[A, C] {
        override def tree(implicit sd: SchemaDescription): FindPair[A, C] =
          Distinct(Chain(left.tree, right.tree)(sa, outerSb, sc, sd))(sa, sc, sd)
      }
    } else  {
      val left = this
      implicit val outerSb = sb
      new RelationalQuery[A, C] {
        override def tree(implicit sd: SchemaDescription): FindPair[A, C] =
          Distinct(Chain(AndRight(left.tree, Find(middle)), right.tree)(sa, outerSb, sc, sd))(sa, sc, sd)
    }
  }

  // union two queries
  def union(right: RelationalQuery[A, B]): RelationalQuery[A, B] = {
    val left = this
    new RelationalQuery[A, B] {
      override def tree(implicit sd: SchemaDescription): FindPair[A, B] = Or(left.tree, right.tree)
    }
  }

  // And two queries
  def intersection(right: RelationalQuery[A, B]): RelationalQuery[A, B] = {
    val left = this
    new RelationalQuery[A, B] {
      override def tree(implicit sd: SchemaDescription): FindPair[A, B] = And(left.tree, right.tree)
    }
  }

  def reverse: RelationalQuery[B, A] = {
    val left = this
    new RelationalQuery[B, A] {
      override def tree(implicit sd: SchemaDescription): FindPair[B, A] = left.tree.reverse
    }
  }

  def leftAnd(f: FindSingle[A]): RelationalQuery[A, B] = {
    val left = this
    new RelationalQuery[A, B] {
      override def tree(implicit sd: SchemaDescription): FindPair[A, B] = Chain(AndRight(FindIdentity[A](), f), left.tree)
    }
  }

  def rightAnd(f: FindSingle[B]): RelationalQuery[A, B] = {
    val left = this
    new RelationalQuery[A, B] {
      override def tree(implicit sd: SchemaDescription): FindPair[A, B] = AndRight(left.tree, f)
    }
  }

}

object RelationalQuery {
  implicit class SymmetricQuery[A](u: RelationalQuery[A, A]) {
    def *(n: Int): RelationalQuery[A, A] = new RelationalQuery[A, A]()(u.sa, u.sa) {
      override def tree(implicit sd: SchemaDescription): FindPair[A, A] = Exactly(n, u.tree)(u.sa, sd)
    }

    def * (r: Repetition): RelationalQuery[A, A] = r match {
      case BetweenRange(lo, hi) => new RelationalQuery[A, A]()(u.sa, u.sa) {
        override def tree(implicit sd: SchemaDescription): FindPair[A, A] = {
          Between(lo, hi, u.tree)(u.sa, sd)
        }
      }

      case UptoRange(n) => new RelationalQuery[A, A]()(u.sa, u.sa) {
        override def tree(implicit sd: SchemaDescription): FindPair[A, A] = {
          Upto(n, u.tree)(u.sa, sd)
        }
      }
      case AtleastRange(n) => new RelationalQuery[A, A]()(u.sa, u.sa) {
        override def tree(implicit sd: SchemaDescription): FindPair[A, A] = {
          Atleast(n, u.tree)(u.sa, sd)
        }
      }
    }

    def ** : RelationalQuery[A, A] = new RelationalQuery[A, A]()(u.sa, u.sa) {
      override def tree(implicit sd: SchemaDescription): FindPair[A, A] = Atleast(0, u.tree)(u.sa, sd)
    }

    def ++ : RelationalQuery[A, A] = new RelationalQuery[A, A]()(u.sa, u.sa) {
      override def tree(implicit sd: SchemaDescription): FindPair[A, A] = Atleast(1, u.tree)(u.sa, sd)
    }

    def ? : RelationalQuery[A, A] = new RelationalQuery[A, A]()(u.sa, u.sa) {
      override def tree(implicit sd: SchemaDescription): FindPair[A, A] = Upto(1, u.tree)(u.sa, sd)
    }
  }

  def emptyRelation[A](implicit sa: SchemaObject[A]): RelationalQuery[A, A] = new RelationalQuery[A, A] {
    override def tree(implicit sd: SchemaDescription): FindPair[A, A] = intermediate.FindIdentity[A]()(sa, sd)
  }

  implicit def relationMonoid[A](implicit sa: SchemaObject[A], sd: SchemaDescription) = new Monoid[RelationalQuery[A, A]] {
    override def zero: RelationalQuery[A, A] = emptyRelation[A](sa)

    override def append(f1: RelationalQuery[A, A], f2: => RelationalQuery[A, A]): RelationalQuery[A, A] = f1 -->--> f2
  }


}