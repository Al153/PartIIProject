package core.dsl

import core.intermediate._
import core.{RelationAttributes, Singleton}
import schema.{Findable, SchemaObject, _}


/**
  * Created by Al on 02/10/2017.
  */
abstract class UnaryQuery[A](implicit sa: SchemaObject[A]) extends RelationalQuery[Singleton, A]()(Singleton.SingletonSchema, sa) {


  private[dsl] def unaryPlus[B](left: UnaryQuery[A], middle: Findable[A], right: RelationalQuery[A, B])(implicit sb: SchemaObject[B]) = new UnaryQuery[B] {
    override def tree: FindPair[Singleton, B] = ???
  }

  def +[B](that: RelationalQuery[A, B])(implicit sb: SchemaObject[B]): UnaryQuery[B] = unaryPlus(this, ??(sa), that)
  def -(a: Findable[A]) = HalfUnaryQuery(this, a)

  def -->[B](that: RelationAttributes[A, B])(implicit sb: SchemaObject[B]): UnaryQuery[B] = unaryPlus(this, ??(sa), that)

  case class HalfUnaryQuery(left: UnaryQuery[A], middle: Findable[A]) {
    def ->[B](that: RelationAttributes[A, B])(implicit sb: SchemaObject[B]): UnaryQuery[B] = left.unaryPlus(left, middle, that)
  }
}

object UnaryQuery {
  implicit class fromAttrs[A](underlying: RelationAttributes[Singleton, A])(implicit sa: SchemaObject[A]) extends UnaryQuery[A] {
    override def tree: FindPair[Singleton, A] = ???
  }
}

