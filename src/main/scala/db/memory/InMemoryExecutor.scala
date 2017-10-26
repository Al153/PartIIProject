package db.memory

import core.RelationAttributes
import core.concrete.relations.CompletedRelation
import core.containers.{Operation, Path}
import core.dsl.RelationalQuery
import core.error.E
import core.intermediate._
import db.interfaces.{DBExecutor, Extractor}
import schema.{RelationName, SchemaObject}
import utils._
import view.View

import scala.concurrent.ExecutionContext


/**
  * Created by Al on 22/10/2017.
  */
class InMemoryExecutor extends DBExecutor {
  override def findAll[A](q: FindSingle[A])(implicit e: ExecutionContext, ea: Extractor[A]): Operation[E, Vector[A]] =
    readOp(
      t =>
        for {
          v <- findSingle(q.getUnsafe, t)
          res <- EitherOps.sequence(v.map(o => ea.fromRow(o.value)))
        } yield res
    )

  override def findAllPairs[A, B](q: FindPair[A, B])(implicit e: ExecutionContext, ea: Extractor[A], eb: Extractor[B]): Operation[E, Vector[(A, B)]] =
    readOp {
      t =>
        for {
          initial <- findSingle(Find(q.sa.generalPattern)(q.sa).getUnsafe, t)
          v <- findPairs(q.getUnsafe, initial, t)
          res <- EitherOps.sequence(
            v.map {
              case (l, r) =>
                for {
                  a <- ea.fromRow(l.value)
                  b <- eb.fromRow(r.value)
                } yield (a, b)
            })
        } yield res
    }

  override def findDistinct[A](q: FindSingle[A])(implicit e: ExecutionContext, ea: Extractor[A]): Operation[E, Set[A]] =
    readOp(
      t =>
        for {
          v <- findSingleSet(q.getUnsafe, t)
          res <- EitherOps.sequence(v.map(o => ea.fromRow(o.value)))
        } yield res
    )

  override def findDistinctPairs[A, B](q: FindPair[A, B])(implicit e: ExecutionContext, ea: Extractor[A], eb: Extractor[B]): Operation[E, Set[(A, B)]] =
    readOp {
      t =>
        for {
          initial <- findSingleSet(Find(q.sa.generalPattern)(q.sa).getUnsafe, t)
          v <- findPairsSet(q.getUnsafe, initial, t)
          res <- EitherOps.sequence(
            v.map {
              case (l, r) =>
                for {
                  a <- ea.fromRow(l.value)
                  b <- eb.fromRow(r.value)
                } yield (a, b)
            })
        } yield res
    }

  override def shortestPath[A](start: A, end: A, relationalQuery: RelationalQuery[A, A])(implicit e: ExecutionContext, sa: SchemaObject[A]): Operation[E, Option[Path[A]]] = ???

  override def allShortestPaths[A](start: A, relationalQuery: RelationalQuery[A, A])(implicit e: ExecutionContext, sa: SchemaObject[A]): Operation[E, Set[Path[A]]] = ???

  override def insert[A, B](t: TraversableOnce[CompletedRelation[A, B]])(implicit sa: SchemaObject[A], sb: SchemaObject[B]): Operation[E, Unit] = ???







  val relationNames: Map[RelationAttributes[Any, Any], RelationName] = ???
  var memoryStore: Map[View, MemoryTree] = Map()






}
