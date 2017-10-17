import core.{NodeDef, RelationAttributes, Singleton}
import core.concrete.relations.CompletedRelation
import core.dsl.RelationSyntax._
import core.dsl.{RelationalQuery, UnaryQuery}
import core.intermediate.IntermediateTree
import db.interfaces.DBExecutor
import examples.Schema.{Actor, ActsIn, Genre, LinkedToTomCruise, Movie, _}
import schema.Pattern
import schema._

import scalaz._
import Scalaz._
import scala.concurrent.ExecutionContext
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
    val m: Findable[Movie] = ?(movieSchema)(None, g.some.map(_.g))
    ActsIn --> m  <-- ActsIn
  }

  val getLinked: UnaryQuery[Schema.Actor] = point --> LinkedToTomCruise

  implicit def dbExecutor = new DBExecutor {override def insert[A <: NodeDef, B <: NodeDef](t: TraversableOnce[CompletedRelation[A, B, RelationAttributes[A, B]]]) = ???

    override def findAll[A](t: IntermediateTree[A])(implicit e: ExecutionContext) = ???

    override def findDistinct[A](t: IntermediateTree[A])(implicit e: ExecutionContext) = ???

    override def allShortestPaths[A <: NodeDef](start: A, relationalQuery: RelationalQuery[A, A])(implicit e: ExecutionContext) = ???

    override def shortestPath[A <: NodeDef](start: A, end: A, relationalQuery: RelationalQuery[A, A])(implicit e: ExecutionContext) = ???
  }

}
