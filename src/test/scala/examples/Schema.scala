package examples

import core.{NodeDef, RelationAttributes}
import schema.Pattern.{?, Pattern, Pattern1, Pattern2}
import schema.{SchemaObject, SchemaObject0, SchemaObject1, SchemaObject2, Singleton}

/**
  * Created by Al on 15/10/2017.
  */
object Schema {
  case class Actor(n: String) extends NodeDef { def name: String = n}
  case class Movie(n: String, g: Genre) extends NodeDef { def name: String = n}
  case object ActsIn extends RelationAttributes[Actor, Movie]
  case class Genre(g: String) extends NodeDef // todo: this should be more typesafe to avoid mispellings - need a tagged table type

  case class Date(year: Int, month: Int, day: Int) extends NodeDef
  case object Birthday extends RelationAttributes[Actor, Date]

  sealed trait Country extends NodeDef // todo: tagged type
  case object UK extends Country
  case object USA extends Country
  /*
    ...
   */

  case object hasNationality extends RelationAttributes[Actor, Country]

  case object Borders extends RelationAttributes[Country, Country]

  /*
   * This is getting into category theory; we define a set of objects  by
   * a relation (morphism) to each member object from the singleton
   */

  case object LinkedToTomCruise extends RelationAttributes[Singleton, Actor]

  implicit def actorConv: Actor => SchemaObject[Actor] = a => SchemaObject1[String, Actor](a.n, Actor.apply)
  implicit def movieConv: Movie => SchemaObject[Movie] =  m => SchemaObject2[String, Genre, Movie](m.n, m.g, {case (s, g) => Movie(s, g)})
  implicit def actsConv: ActsIn.type  => SchemaObject[ActsIn.type] = _ => SchemaObject0[ActsIn.type](() => ActsIn)

  implicit def actorPattern: Pattern[Actor] = Pattern1[Actor, String](?[String])
  implicit def moviePattern: Pattern[Movie] = Pattern2[Movie, String, Genre](?[String], ?[Genre])
  implicit def genrePattern: Pattern[Genre] = Pattern1[Genre, String](?[String])


}