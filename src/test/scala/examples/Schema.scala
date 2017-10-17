package examples

import core.{NodeDef, RelationAttributes, Singleton}
import prototyping._

/**
  * Created by Al on 15/10/2017.
  */
object Schema {
  case class Actor(n: String) extends NodeDef { def name: String = n}
  case class Movie(n: String, g: Genre) extends NodeDef { def name: String = n}

  case class Genre(g: String) extends NodeDef // todo: this should be more typesafe to avoid mispellings - need a tagged table type

  case class Date(year: Int, month: Int, day: Int) extends NodeDef


  sealed trait Country extends NodeDef // todo: tagged type
  case object UK extends Country
  case object USA extends Country
  case object NoCountry extends Country
  /*
    ...
   */
  case object ActsIn extends RelationAttributes[Actor, Movie]
  case object Birthday extends RelationAttributes[Actor, Date]
  case object hasNationality extends RelationAttributes[Actor, Country]
  case object Borders extends RelationAttributes[Country, Country]

  /*
   * This is getting into category theory; we define a set of objects  by
   * a relation (morphism) to each member object from the singleton
   */

  case object LinkedToTomCruise extends RelationAttributes[Singleton, Actor]

  implicit def actorSchema = new SchemaObject1[Actor, String] {
    override def construct(a1: String): Actor = Actor(a1)
    override def name: TableName = TableName("Actors")
    override def toTuple(a: Actor): DBTuple1[Actor, String] = DBTuple1(a.name)
  }

  implicit def movieSchema = new prototyping.SchemaObject2[Movie, String, String] {
    override def construct(a1: String, a2: String): Movie = Movie(a1, Genre(a2))
    override def name: TableName = TableName("Movies")
    override def toTuple(m: Movie): DBTuple2[Movie, String, String] = DBTuple2(m.name, m.g.g)
  }

  implicit def DateSchema = new SchemaObject3[Date, Int, Int, Int] {
    override def construct(year: Int, month: Int, day: Int): Date = Date(year, month, day)
    override def name: TableName = TableName("Dates")
    override def toTuple(a: Date): DBTuple3[Date, Int, Int, Int] = DBTuple3(a.year, a.month, a.day)
  }

  implicit def CountrySchema = new SchemaObject1[Country, String] {
    private val lookup: Map[String, Country] = Map("uk" -> UK, "usa" -> USA, "n/a" -> NoCountry)
    private val reverseLookup = lookup.map { case (a, b) => b -> a }
    override def construct(a1: String): Country = lookup.getOrElse(a1, NoCountry)
    override def name: TableName = TableName("Countries")
    override def toTuple(a: Country): DBTuple1[Country, String] = DBTuple1(reverseLookup.getOrElse(a, "n/a"))
  }


}