package construction.imdb

import core.user.dsl.Relation
import core.user.schema._


/**
  * Created by Al on 05/01/2018.
  */
object IMDBSchema {
  case class Movie(name: String, language: String)
  case class Person(name: String)
  case class Date(time: Long)
  case class Place(name: String)
  case class Genre(name: String)



  implicit def personSchema = new SchemaObject1[Person, String] {
    override def construct(a1: String): Person = Person(a1)
    override def name: TableName = TableName("People")
    override def toTuple(a: Person): SchemaObject[Person] => DBTuple1[Person, String] = buildDBTuple(a.name)
  }

  implicit def movieSchema = new SchemaObject2[Movie, String, String] {
    override def construct(name: String, language: String): Movie = Movie(name, language)
    override def name: TableName = TableName("Movies")
    override def toTuple(a: Movie): SchemaObject[Movie] => DBTuple2[Movie, String, String] = buildDBTuple(a.name, a.language)
  }


  implicit def dateSchema = new SchemaObject2[Date, Int, Int] {
    override def construct(top: Int, bottom: Int): Date = Date((top: Long) << 32 + (bottom: Long))
    override def name: TableName = TableName("Date")
    override def toTuple(a: Date): SchemaObject[Date] => DBTuple2[Date, Int, Int] = buildDBTuple((a.time >> 32).toInt, a.time.toInt)
  }


  implicit def placeSchema = new SchemaObject1[Place, String] {
    override def construct(a1: String): Place = Place(a1)
    override def name: TableName = TableName("People")
    override def toTuple(a: Place): SchemaObject[Place] =>DBTuple1[Place, String] = buildDBTuple(a.name)
  }


  implicit def genreSchema = new SchemaObject1[Genre, String] {
    override def construct(a1: String): Genre = Genre(a1)
    override def name: TableName = TableName("People")
    override def toTuple(a: Genre): SchemaObject[Genre] => DBTuple1[Genre, String] = buildDBTuple(a.name)
  }


  case object ActsIn extends Relation[Person, Movie]
  case object Directed extends Relation[Person, Movie]
  case object HasBirthday extends Relation[Person,Date]
  case object BornIn extends Relation[Person, Place]
  case object HasGenre extends Relation[Movie, Genre]

  implicit val schemaDescription = new SchemaDescription(
    Set(personSchema, movieSchema, dateSchema, placeSchema, genreSchema),
    Set(ActsIn, Directed, BornIn, HasBirthday, HasGenre)
  )


}
