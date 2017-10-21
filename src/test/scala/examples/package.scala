import core.concrete.relations.CompletedRelation
import core.containers.{Operation, Path}
import core.dsl.RelationSyntax._
import core.dsl.{RelationalQuery, UnaryQuery}
import core.error.E
import core.intermediate.IntermediateTree
import core.{RelationAttributes, Singleton}
import db.interfaces.DBExecutor
import examples.Schema.{Actor, ActsIn, Genre, LinkedToTomCruise, Movie, _}
import schema._
import db.interfaces.Extractor

import scala.concurrent.ExecutionContext
import scalaz.Scalaz._
import scalaz._
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
    ActsIn --> ?(movieSchema)(None, g.some.map(_.g)) <-- ActsIn
  }

  val getLinked: UnaryQuery[Schema.Actor] = point --> LinkedToTomCruise

  implicit def dbExecutor = new DBExecutor {
    override def insert[A, B](t: TraversableOnce[CompletedRelation[A, B, RelationAttributes[A, B]]])(implicit sa: SchemaObject[A], sb: SchemaObject[B]): Operation[E, Unit] = ???

    override def findAll[A](t: IntermediateTree[A])(implicit e: ExecutionContext, ea: Extractor[A]) = ???

    override def findDistinct[A](t: IntermediateTree[A])(implicit e: ExecutionContext) = ???

    override def allShortestPaths[A](start: A, relationalQuery: RelationalQuery[A, A])(implicit e: ExecutionContext, sa: SchemaObject[A]): Operation[E, Set[Path[A]]] = ???

    override def shortestPath[A](start: A, end: A, relationalQuery: RelationalQuery[A, A])(implicit e: ExecutionContext, sa: SchemaObject[A]): Operation[E, Option[Path[A]]] = ???
  }

}
