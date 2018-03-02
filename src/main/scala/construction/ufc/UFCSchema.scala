package construction.ufc

import core.user.dsl.Relation
import core.user.schema._

/**
  * Created by Al on 21/01/2018.
  */
object UFCSchema {
  case class Person(name: String, height: Int, weight: Int)

  implicit object PersonSchema extends SchemaObject3[Person, String, Int, Int] {
    /**
      * Need to define a constructor for an [[Person]] from the components gained from the table
      */
    override def construct(name: String, height: Int, weight: Int): Person = Person(name, height, weight)

    /**
      * Need to define the expected name of the table
      */
    override def name: TableName = TableName("Person")

    /**
      * Need to define how to convert a [[Person]] into some storeable primitives
      */
    override def toTuple(a: Person): (SchemaObject[Person]) => DBTuple3[Person, String, Int, Int] = buildDBTuple(a.name, a.height, a.weight)
  }

  case object Beat extends Relation[Person, Person]
  case object ShorterThan extends Relation[Person, Person]
  case object LighterThan extends Relation[Person, Person]

  implicit val schema = new SchemaDescription(
    Set(PersonSchema),
    Set(Beat, ShorterThan, LighterThan)
  )
}
