package core

import core.dsl.UnaryQuery
import core.intermediate.IntermediateTree
import schema.{DBTuple0, SchemaObject, SchemaObject0, TableName}


/**
  * Created by Al on 09/10/2017.
  */
case class Singleton() extends UnaryQuery[Singleton] with NodeDef {
  override def tree(a: IntermediateTree[Singleton]): IntermediateTree[(Singleton, Singleton)] = ???

  override def apply: IntermediateTree[Singleton] = ???
}

object Singleton {
  val point = Singleton()

  implicit def SingletonSchema = new SchemaObject0[Singleton] {
    override def construct(): Singleton = point

    override def name: TableName = TableName("Singleton")

    override def toTuple(a: Singleton): DBTuple0[Singleton] = new DBTuple0[Singleton]
  }
}
