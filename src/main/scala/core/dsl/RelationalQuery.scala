package core.dsl

import core.NodeDef
import core.intermediate._
import schema.Pattern.Pattern
import RelationSyntax._

import scalaz._, Scalaz._

/**
  * Created by Al on 12/10/2017.
  */
abstract class RelationalQuery[A <: NodeDef, B <: NodeDef]
{
  def tree(a: IntermediateTree[A]): IntermediateTree[(A, B)]

  def from(a: Pattern[A]): IntermediateTree[B] = {
    fromPattern(a)

    ???
  }


  // chain together two queries in sequence
  private[RelationalQuery] def plus[C <: NodeDef](middle: Pattern[B], right: RelationalQuery[B, C]) = {
      val left = this
      new RelationalQuery[A, C] {
        override def tree(a: IntermediateTree[A]): IntermediateTree[(A, C)] =
          Chain[A, B, C](Narrow(left.tree(a), middle), right)
      }
    }

  // union two queries
  private[RelationalQuery] def union(right: RelationalQuery[A, B]) = {
    val left = this
    new RelationalQuery[A, B] {
      override def tree(a: IntermediateTree[A]): IntermediateTree[(A,B)] = Union(left.tree(a), right.tree(a))
    }
  }

  // And two queries
  private[RelationalQuery] def intersection(right: RelationalQuery[A, B]) = {
    val left = this
    new RelationalQuery[A, B] {
      override def apply(a: IntermediateTree[A]): IntermediateTree[(A, B)] = Intersection(left(a), right(a))
    }
  }

  def reverse: RelationalQuery[B, A] = ??? // todo

  def pair: IntermediateTree[(A, B)] = ???


}

object RelationalQuery {
  implicit case class SymmetricQuery[A <: NodeDef](u: RelationalQuery[A, A])(implicit monoid: Monoid[RelationalQuery[A, A]]) {
    def |*|(n: Int): RelationalQuery[A, A] = List.fill(n)(u).fold(emptyRelation)(_ |+| _)
    def * : RelationalQuery[A, A] = ???
    def ? : RelationalQuery[A, A] = u.union(emptyRelation)
  }

  def emptyRelation[A <: NodeDef] = new RelationalQuery[A, A] {
    override def tree(a: IntermediateTree[A]): IntermediateTree[(A, A)] = Pass[(A, A)]()
  }

  implicit def relationMonoid[A <: NodeDef] = new Monoid[RelationalQuery[A, A]] {
    override def zero: RelationalQuery[A, A] = emptyRelation[A]

    override def append(f1: RelationalQuery[A, A], f2: => RelationalQuery[A, A]): RelationalQuery[A, A] = f1 -->--> f2
  }


}