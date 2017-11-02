package core.dsl

import core.intermediate._
import core.{RelationAttributes, Singleton}
import schema.{Findable, SchemaObject, _}


/**
  * Created by Al on 02/10/2017.
  */
abstract class UnaryQuery[A](implicit sa: SchemaObject[A]) extends RelationalQuery[Singleton, A]()(Singleton.SingletonSchema, sa) {


  private[dsl] def unaryPlus[B](middle: Findable[A], right: RelationalQuery[A, B])(implicit sb: SchemaObject[B]) = {
    val left = this
    implicit val outerSa = sa
    new UnaryQuery[B] {
      override def tree: FindPair[Singleton, B] =
        Chain(Narrow(left.tree, middle), right.tree)(Singleton.SingletonSchema, outerSa, sb)
    }
  }

  def find: FindSingle[A] = From(Find(Singleton.findable), this.tree)

  def +[B](that: RelationalQuery[A, B])(implicit sb: SchemaObject[B]): UnaryQuery[B] = unaryPlus(sa.generalPattern, that)
  def -(a: Findable[A]) = HalfUnaryQuery(this, a)

  def -->[B](that: RelationAttributes[A, B])(implicit sb: SchemaObject[B]): UnaryQuery[B] = unaryPlus(sa.generalPattern, that)

  case class HalfUnaryQuery(left: UnaryQuery[A], middle: Findable[A]) {
    def ->[B](that: RelationAttributes[A, B])(implicit sb: SchemaObject[B]): UnaryQuery[B] = left.unaryPlus(middle, that)
  }
}

