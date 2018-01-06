package core.user.dsl

import core.backend.intermediate.{FindPair, Id}
import core.user.schema._



/**
  * Created by Al on 09/10/2017.
  */

// todo: is this actually used?
case class Singleton() extends UnaryQuery[Singleton]()(Singleton.SingletonSchema) {
  override def tree(implicit sd: SchemaDescription): FindPair[Singleton, Singleton] = Id()(Singleton.SingletonSchema, sd)
}

object Singleton {
  val point = Singleton()

  val findable: Findable[Singleton] = Singleton.SingletonSchema.findable(Singleton.point)

  implicit def SingletonSchema: SchemaObject0[Singleton] = new SchemaObject0[Singleton] {
    override def construct(): Singleton = point

    override def tableName: TableName = TableName("Singleton")

    override def toTuple(a: Singleton): DBTuple0[Singleton] = new DBTuple0[Singleton](tableName)
  }
}