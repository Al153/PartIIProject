import core.dsl.RelationSyntax._
import core.dsl.UnaryQuery
import examples.Schema.{Actor, ActsIn, Genre, LinkedToTomCruise, Movie, _}
import schema.Pattern.{?, Pattern}
import schema.Singleton

/**
  * Created by Al on 15/10/2017.
  */
package object examples {
  val point = Singleton()


  /*
 * Define some useful relations using the algebraic DSL
 */

  val tomCruise = Actor("Tom cruise")
  val jenniferLawrence = Actor("Jennifer Lawrence")

  val coactor = ActsIn --><-- ActsIn
  val sameBirthday = Birthday --><-- Birthday
  val colocated = hasNationality --><-- hasNationality

  val produced = hasNationality.reverse -->--> ActsIn

  def coactorWithGenre(g: Genre) = {
    val m: Pattern[Movie] = ?[Movie].apply(?, g)
    ActsIn --> m <-- ActsIn
  }

  val getLinked: UnaryQuery[Schema.Actor] = point --> LinkedToTomCruise

}
