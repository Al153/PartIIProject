package db.memory

import core.RelationAttributes
import core.concrete.relations.CompletedRelation
import core.containers.{Operation, Path}
import core.dsl.RelationalQuery
import core.error.E
import core.intermediate._
import core.intermediate.unsafe._
import db.common._
import db.interfaces.{DBExecutor, Extractor}
import schema.{Findable, RelationName, SchemaObject, TableName}
import view.View
import utils._

import scala.concurrent.ExecutionContext
import scalaz.Scalaz._
import scalaz._


/**
  * Created by Al on 22/10/2017.
  */
class InMemoryExecutor extends DBExecutor {
  override def findAll[A](q: FindSingle[A])(implicit e: ExecutionContext, extractor: Extractor[A]): Operation[E, Vector[A]] = readOp(t => findSingle(q.getUnsafe, t).flatMap(extractor.fromRow))

  override def findAllPairs[A, B](t: FindPair[A, B])(implicit e: ExecutionContext, ea: Extractor[A], eb: Extractor[B]): Operation[E, Vector[(A, B)]] = readOp(???)

  override def findDistinct[A](t: FindSingle[A])(implicit e: ExecutionContext): Operation[E, Set[A]] = ???

  override def findDistinctPairs[A, B](t: FindPair[A, B])(implicit e: ExecutionContext, ea: Extractor[A], eb: Extractor[B]): Operation[E, Vector[(A, B)]] = ???

  override def shortestPath[A](start: A, end: A, relationalQuery: RelationalQuery[A, A])(implicit e: ExecutionContext, sa: SchemaObject[A]): Operation[E, Option[Path[A]]] = ???

  override def allShortestPaths[A](start: A, relationalQuery: RelationalQuery[A, A])(implicit e: ExecutionContext, sa: SchemaObject[A]): Operation[E, Set[Path[A]]] = ???

  override def insert[A, B](t: TraversableOnce[CompletedRelation[A, B]])(implicit sa: SchemaObject[A], sb: SchemaObject[B]): Operation[E, Unit] = ???







  val relationNames: Map[RelationAttributes[Any, Any], RelationName] = ???
  var memoryStore: Map[View, MemoryTree] = Map()






}
