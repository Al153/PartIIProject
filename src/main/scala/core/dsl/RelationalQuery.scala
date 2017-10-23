package core.dsl


import core.intermediate._
import RelationSyntax._
import schema.{Findable, SchemaObject}

import scalaz._
import Scalaz._

/**
  * Created by Al on 12/10/2017.
  */
abstract class RelationalQuery[A, B](implicit sa: SchemaObject[A], sb: SchemaObject[B]) {
  def tree: FindPair[A, B]

  def from(a: Findable[A])(implicit schema: SchemaObject[A]): FindSingle[B] = {
    From(Find(a), tree)
  }


  // chain together two queries in sequence
  private[dsl] def plus[C](middle: Findable[B], right: RelationalQuery[B, C])(implicit sc: SchemaObject[C]) = {
    val left = this
    new RelationalQuery[A, C] {
      override def tree: FindPair[A, C] =
        Chain(Narrow(left.tree, middle), right.tree)
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
    def |*|(n: Int): RelationalQuery[A, A] = new RelationalQuery[A, A] {
      override def tree: FindPair[A, A] = Exactly(n, u.tree)
    }
    def * : RelationalQuery[A, A] = new RelationalQuery[A, A] {
      override def tree: FindPair[A, A] = AtLeast(0, u.tree)
    }
    def ? : RelationalQuery[A, A] = new RelationalQuery[A, A] {
      override def tree: FindPair[A, A] = Upto(1, u.tree)
    }
  }

  def emptyRelation[A](implicit sa: SchemaObject[A]): RelationalQuery[A, A] = new RelationalQuery[A, A] {
    override def tree: FindPair[A, A] = Pass[A]
  }

  implicit def relationMonoid[A](implicit schema: SchemaObject[A]) = new Monoid[RelationalQuery[A, A]] {
    override def zero: RelationalQuery[A, A] = emptyRelation[A](schema)

    override def append(f1: RelationalQuery[A, A], f2: => RelationalQuery[A, A]): RelationalQuery[A, A] = f1 -->--> f2
  }


}