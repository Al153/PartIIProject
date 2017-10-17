package core.dsl

import core.intermediate._
import core.{NodeDef, RelationAttributes, Singleton}
import schema.{Findable, SchemaObject, _}


/**
  * Created by Al on 02/10/2017.
  */
abstract class UnaryQuery[A <: NodeDef] extends RelationalQuery[Singleton, A] {
  override def tree(a: IntermediateTree[Singleton]): IntermediateTree[(Singleton, A)] = Chain(Singleton.point.tree(a), this)
  def apply: IntermediateTree[A]

  private[dsl] def unaryPlus[B <: NodeDef](left: UnaryQuery[A], middle: Findable[A], right: RelationalQuery[A, B]) = new UnaryQuery[B] {
    override def apply: IntermediateTree[B] = Proj2(Chain(Dup(left.apply), right))
  }

  def +[B <: NodeDef](that: RelationalQuery[A, B])(implicit aSchema: SchemaObject[A]): UnaryQuery[B] = unaryPlus(this, ??(aSchema), that)
  def -[B <: NodeDef, R](a: Findable[A])(implicit f: R => RelationalQuery[A, B]) = HalfUnaryQuery(this, a)

  def -->[B <: NodeDef, R](that: R)(implicit f: R => RelationalQuery[A, B], aSchema: SchemaObject[A]): UnaryQuery[B] = unaryPlus(this, ??(aSchema), that)

  case class HalfUnaryQuery[B <: NodeDef](left: UnaryQuery[A], middle: Findable[A]) {
    def ->[R](that: R)(implicit f: R => RelationalQuery[A, B]): UnaryQuery[B] = left.unaryPlus(left, middle, that)
  }
}

object UnaryQuery {
  implicit class fromAttrs[A <: NodeDef](underlying: RelationAttributes[Singleton, A]) extends UnaryQuery[A] {
    override def apply: IntermediateTree[A] = ???
  }
}

