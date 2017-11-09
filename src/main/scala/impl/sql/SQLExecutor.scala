package impl.sql

import core.backend.interfaces.{DBExecutor, Extractor}
import core.containers.{Operation, Path}
import core.dsl.RelationalQuery
import core.error.E
import core.intermediate.{FindPair, FindSingle}
import core.relations.CompletedRelation
import core.schema.{SchemaDescription, SchemaObject}

import scala.concurrent.ExecutionContext

class SQLExecutor(instance: SQLInstance) extends DBExecutor {
  override def findAll[A](t: FindSingle[A])
                         (implicit e: ExecutionContext, extractor: Extractor[A], sd: SchemaDescription): Operation[E, Vector[A]] = ???

  override def findAllPairs[A, B](t: FindPair[A, B])
                                 (implicit e: ExecutionContext, ea: Extractor[A], eb: Extractor[B], sd: SchemaDescription): Operation[E, Vector[(A, B)]] = ???

  override def findDistinct[A](t: FindSingle[A])
                              (implicit e: ExecutionContext, extractor: Extractor[A], sd: SchemaDescription): Operation[E, Set[A]] = ???

  override def findDistinctPairs[A, B](t: FindPair[A, B])
                                      (implicit e: ExecutionContext, ea: Extractor[A], eb: Extractor[B], sd: SchemaDescription): Operation[E, Set[(A, B)]] = ???

  override def shortestPath[A](start: A, end: A, relationalQuery: RelationalQuery[A, A])
                              (implicit e: ExecutionContext, sa: SchemaObject[A], sd: SchemaDescription): Operation[E, Option[Path[A]]] = ???

  override def allShortestPaths[A](start: A, relationalQuery: RelationalQuery[A, A])
                                  (implicit e: ExecutionContext, sa: SchemaObject[A], sd: SchemaDescription): Operation[E, Set[Path[A]]] = ???

  override def insert[A, B](t: TraversableOnce[CompletedRelation[A, B]])(
    implicit e: ExecutionContext, sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription): Operation[E, Unit] = ???
}