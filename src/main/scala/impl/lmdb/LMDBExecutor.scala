package impl.lmdb

import core.backend.interfaces.DBExecutor
import core.containers.{Operation, Path}
import core.dsl.RelationalQuery
import core.error.E
import core.intermediate.{FindPair, FindSingle}
import core.relations.CompletedRelation
import core.schema.{SchemaDescription, SchemaObject}

/**
  * Created by Al on 12/12/2017.
  */
class LMDBExecutor extends DBExecutor {
  override def findAll[A](t: FindSingle[A])(implicit extractor: SchemaObject[A], sd: SchemaDescription): Operation[E, Vector[A]] = ???

  override def findAllPairs[A, B](t: FindPair[A, B])(implicit sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription): Operation[E, Vector[(A, B)]] = ???

  override def findDistinct[A](t: FindSingle[A])(implicit extractor: SchemaObject[A], sd: SchemaDescription): Operation[E, Set[A]] = ???

  override def findDistinctPairs[A, B](t: FindPair[A, B])(implicit sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription): Operation[E, Set[(A, B)]] = ???

  override def shortestPath[A](start: A, end: A, relationalQuery: RelationalQuery[A, A])(implicit sa: SchemaObject[A], sd: SchemaDescription): Operation[E, Option[Path[A]]] = ???

  override def allShortestPaths[A](start: A, relationalQuery: RelationalQuery[A, A])(implicit sa: SchemaObject[A], sd: SchemaDescription): Operation[E, Set[Path[A]]] = ???

  override def insert[A, B](t: TraversableOnce[CompletedRelation[A, B]])(implicit sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription): Operation[E, Unit] = ???
}
