package core.dsl

import core.{NodeDef, RelationAttributes}
import core.intermediate.{Chain, GetNode, GetRelation, IntermediateTree, Intersection, Join, Proj2}
import schema.Pattern.Pattern
import schema.Pattern._
import schema.Singleton

/**
  * Created by Al on 02/10/2017.
  */
abstract class UnaryQuery[A <: NodeDef] extends RelationalQuery[Singleton, A] {
  override def apply(a: IntermediateTree[Singleton]): IntermediateTree[(Singleton, A)] = Chain(Singleton.apply(a), this)
  def apply: IntermediateTree[A]

  private def plus[B <: NodeDef](left: UnaryQuery[A], middle: Pattern[A], right: RelationalQuery[A, B]) = new UnaryQuery[B] {
    override def apply: IntermediateTree[B] = Proj2(right.apply(Intersection(left.apply, middle)))
  }

  def +[B <: NodeDef](that: RelationalQuery[A, B]): UnaryQuery[B] = plus(this, ?[A], that)
  def -[B <: NodeDef, R](a: Pattern[A])(implicit f: R => RelationalQuery[A, B]) = {
    val outer = this
    new Object {
      def ->(r: R): UnaryQuery[B] = outer.plus(outer, a, r)
    }
  }
  def -->[B <: NodeDef, R](that: R)(implicit f: R => RelationalQuery[A, B]): UnaryQuery[B] = plus(this, ?[A], that)

}

object UnaryQuery {
  implicit class fromAttrs[A <: NodeDef](underlying: RelationAttributes[schema.Singleton, A]) extends UnaryQuery[A] {
    override def apply: IntermediateTree[A] = ???


  }
}

