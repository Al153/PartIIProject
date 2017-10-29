package core

import core.dsl.UnaryQuery
import core.intermediate.FindPair
import schema.{DBTuple0, SchemaObject0, TableName}


/**
  * Created by Al on 09/10/2017.
  */
case class Singleton() extends UnaryQuery[Singleton]()(Singleton.SingletonSchema) {
  override def tree: FindPair[Singleton, Singleton] = ???
}

object Singleton {
  val point = Singleton()

  implicit def SingletonSchema: SchemaObject0[Singleton] = new SchemaObject0[Singleton] {
    override def construct(): Singleton = point

    override def tableName: TableName = TableName("Singleton")

    override def toTuple(a: Singleton): DBTuple0[Singleton] = new DBTuple0[Singleton](tableName)
  }
}
