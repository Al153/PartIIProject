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
  def tree(a: IntermediateTree[A]): IntermediateTree[(A, B)]

  def from(a: A)(implicit schema: SchemaObject[A]): IntermediateTree[B] = {
    schema.findable(a)
    ???
  }


  // chain together two queries in sequence
  private[dsl] def plus[C](middle: Findable[B], right: RelationalQuery[B, C])(implicit sc: SchemaObject[C]) = {
      val left = this
      new RelationalQuery[A, C] {
        override def tree(a: IntermediateTree[A]): IntermediateTree[(A, C)] =
          Chain[A, B, C](Narrow(left.tree(a), middle), right)
      }
    }

  // union two queries
  private[dsl] def union(right: RelationalQuery[A, B]) = {
    val left = this
    new RelationalQuery[A, B] {
      override def tree(a: IntermediateTree[A]): IntermediateTree[(A,B)] = Union(left.tree(a), right.tree(a))
    }
  }

  // And two queries
  private[dsl] def intersection(right: RelationalQuery[A, B]) = {
    val left = this
    new RelationalQuery[A, B] {
      override def tree(a: IntermediateTree[A]): IntermediateTree[(A, B)] = Intersection(left.tree(a), right.tree(a))
    }
  }

  def reverse: RelationalQuery[B, A] = ??? // todo

  def pair: IntermediateTree[(A, B)] = ???


}

object RelationalQuery {
  implicit class SymmetricQuery[A](u: RelationalQuery[A, A])(implicit monoid: Monoid[RelationalQuery[A, A]], sa: SchemaObject[A]) {
    def |*|(n: Int): RelationalQuery[A, A] = List.fill(n)(u).fold(emptyRelation)(_ |+| _)
    def * : RelationalQuery[A, A] = ???
    def ? : RelationalQuery[A, A] = u.union(emptyRelation[A](sa))
  }

  def emptyRelation[A](implicit sa: SchemaObject[A]): RelationalQuery[A, A] = new RelationalQuery[A, A] {
    def tree(a: IntermediateTree[A]): IntermediateTree[(A, A)] = Dup(a)
  }

  implicit def relationMonoid[A](implicit schema: SchemaObject[A]) = new Monoid[RelationalQuery[A, A]] {
    override def zero: RelationalQuery[A, A] = emptyRelation[A](schema)

    override def append(f1: RelationalQuery[A, A], f2: => RelationalQuery[A, A]): RelationalQuery[A, A] = f1 -->--> f2
  }


}