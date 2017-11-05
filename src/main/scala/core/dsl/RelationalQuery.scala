package core.dsl


import core.intermediate._
import RelationSyntax._
import core.intermediate
import schema.{Findable, SchemaDescription, SchemaObject}

import scalaz._
import Scalaz._

/**
  * Created by Al on 12/10/2017.
  */
abstract class RelationalQuery[A, B](implicit val sa: SchemaObject[A], val sb: SchemaObject[B]) {
  def tree(implicit sd: SchemaDescription): FindPair[A, B]

  def from(a: Findable[A])(implicit sd: SchemaDescription): FindSingle[B] = {
    From(Find(a), tree)
  }

  def from(a: A)(implicit sd: SchemaDescription) = From(Find(sa.findable(a)), tree)



  // chain together two queries in sequence
  private[dsl] def plus[C](middle: Findable[B], right: RelationalQuery[B, C])(implicit sc: SchemaObject[C]) = {
    val left = this
    implicit val outerSb = sb
    new RelationalQuery[A, C] {
      override def tree(implicit sd: SchemaDescription): FindPair[A, C] =
        Chain(Narrow(left.tree, middle), right.tree)(sa, outerSb, sc, sd)
    }
  }

  private[dsl] def plusDistinct[C](middle: Findable[B], right: RelationalQuery[B, C])(implicit sc: SchemaObject[C]) = {
    val left = this
    implicit val outerSb = sb
    new RelationalQuery[A, C] {
      override def tree(implicit sd: SchemaDescription): FindPair[A, C] =
        Distinct(Chain(Narrow(left.tree, middle), right.tree)(sa, outerSb, sc, sd))(sa, sc, sd)
    }
  }

  // union two queries
  private[dsl] def union(right: RelationalQuery[A, B]) = {
    val left = this
    new RelationalQuery[A, B] {
      override def tree(implicit sd: SchemaDescription): FindPair[A, B] = Or(left.tree, right.tree)
    }
  }

  // And two queries
  private[dsl] def intersection(right: RelationalQuery[A, B]) = {
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

}

object RelationalQuery {
  implicit class SymmetricQuery[A](u: RelationalQuery[A, A]) {
    def *(n: Int): RelationalQuery[A, A] = new RelationalQuery[A, A]()(u.sa, u.sa) {
      override def tree(implicit sd: SchemaDescription): FindPair[A, A] = Exactly(n, u.tree)(u.sa, sd)
    }

    def * (p: (Int, Int)) = new RelationalQuery[A, A]()(u.sa, u.sa) {
      override def tree(implicit sd: SchemaDescription): FindPair[A, A] = {
        val (from, to) = p
        Between(from, to, u.tree)(u.sa, sd)
      }
    }

    def *+ (n: Int) = new RelationalQuery[A, A]()(u.sa, u.sa) {
      override def tree(implicit sd: SchemaDescription): FindPair[A, A] = Atleast(n, u.tree)(u.sa, sd)
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
    override def tree(implicit sd: SchemaDescription): FindPair[A, A] = intermediate.Id[A]()(sa, sd)
  }

  implicit def relationMonoid[A](implicit sa: SchemaObject[A], sd: SchemaDescription) = new Monoid[RelationalQuery[A, A]] {
    override def zero: RelationalQuery[A, A] = emptyRelation[A](sa)

    override def append(f1: RelationalQuery[A, A], f2: => RelationalQuery[A, A]): RelationalQuery[A, A] = f1 -->--> f2
  }


}