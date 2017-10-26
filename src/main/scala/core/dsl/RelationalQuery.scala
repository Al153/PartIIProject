package core.dsl


import core.intermediate._
import RelationSyntax._
import core.intermediate
import schema.{Findable, SchemaObject}

import scalaz._
import Scalaz._

/**
  * Created by Al on 12/10/2017.
  */
abstract class RelationalQuery[A, B](implicit val sa: SchemaObject[A], val sb: SchemaObject[B]) {
  def tree: FindPair[A, B]

  def from(a: Findable[A]): FindSingle[B] = {
    From(Find(a), tree)
  }

  def from(a: A) = {
    From(Find(sa.findable(a)), tree)
  }


  // chain together two queries in sequence
  private[dsl] def plus[C](middle: Findable[B], right: RelationalQuery[B, C])(implicit sc: SchemaObject[C]) = {
    val left = this
    implicit val outerSb = sb
    new RelationalQuery[A, C] {
      override def tree: FindPair[A, C] =
        Chain(Narrow(left.tree, middle), right.tree)(sa, outerSb, sc)
    }
  }

  // union two queries
  private[dsl] def union(right: RelationalQuery[A, B]) = {
    val left = this
    new RelationalQuery[A, B] {
      override def tree: FindPair[A, B] = Or(left.tree, right.tree)
    }
  }

  // And two queries
  private[dsl] def intersection(right: RelationalQuery[A, B]) = {
    val left = this
    new RelationalQuery[A, B] {
      override def tree: FindPair[A, B] = And(left.tree, right.tree)
    }
  }

  def reverse: RelationalQuery[B, A] = {
    val left = this
    new RelationalQuery[B, A] {
      override def tree: FindPair[B, A] = left.tree.reverse
    }
  }

}

object RelationalQuery {
  implicit class SymmetricQuery[A](u: RelationalQuery[A, A]) {
    def |*|(n: Int): RelationalQuery[A, A] = new RelationalQuery[A, A]()(u.sa, u.sa) {
      override def tree: FindPair[A, A] = Exactly(n, u.tree)(u.sa)
    }
    def * : RelationalQuery[A, A] = new RelationalQuery[A, A]()(u.sa, u.sa) {
      override def tree: FindPair[A, A] = Atleast(0, u.tree)(u.sa)
    }
    def ? : RelationalQuery[A, A] = new RelationalQuery[A, A]()(u.sa, u.sa) {
      override def tree: FindPair[A, A] = Upto(1, u.tree)(u.sa)
    }
  }

  def emptyRelation[A](implicit sa: SchemaObject[A]): RelationalQuery[A, A] = new RelationalQuery[A, A] {
    override def tree: FindPair[A, A] = intermediate.Id[A]()(sa)
  }

  implicit def relationMonoid[A](implicit sa: SchemaObject[A]) = new Monoid[RelationalQuery[A, A]] {
    override def zero: RelationalQuery[A, A] = emptyRelation[A](sa)

    override def append(f1: RelationalQuery[A, A], f2: => RelationalQuery[A, A]): RelationalQuery[A, A] = f1 -->--> f2
  }


}