import core.user.dsl._
import core.user.dsl.UnaryQuery
import examples.Schema.{Actor, ActsIn, Genre, LinkedToTomCruise, _}
import core.user.schema._

import scalaz.Scalaz._
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
    ActsIn --> movieSchema.pattern(None, g.g.some) <-- ActsIn
  }

  val getLinked: UnaryQuery[Schema.Actor] = point --> LinkedToTomCruise


}
