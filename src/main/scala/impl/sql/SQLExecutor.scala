package impl.sql

import core.backend.interfaces.DBExecutor
import core.containers.{ConstrainedFuture, Operation, Path}
import core.dsl.RelationalQuery
import core.error.E
import core.intermediate.{FindPair, FindSingle}
import core.relations.CompletedRelation
import core.schema.{SchemaDescription, SchemaObject}
import core.view.View
import impl.sql.adt.{CompletedPairQuery, CompletedSingleQuery}

class SQLExecutor(instance: SQLInstance) extends DBExecutor {

  import instance.executionContext

  override def findAll[A](t: FindSingle[A])
                         (
                           implicit sa: SchemaObject[A],
                           sd: SchemaDescription
                         ): Operation[E, Vector[A]] =
    Operation.readOp {
      v: View =>
        for {
          query <-
          ConstrainedFuture.eitherR(
            for {
              ut <- t.getUnsafe
              table <- instance.lookupTable (ut.table)
              pairQuery = CompletedSingleQuery(ut, table, sd) (instance)
              stringQuery <- pairQuery.render (v)
            } yield stringQuery)

          res <- ConstrainedFuture.either(
            instance
              .reader
              .getSingleObject[A](query)
          )(errors.recoverSQLException)
        } yield res
    }

  override def findAllPairs[A, B](t: FindPair[A, B])
                                 (
                                   implicit sa: SchemaObject[A],
                                   sb: SchemaObject[B],
                                   sd: SchemaDescription
                                 ): Operation[E, Vector[(A, B)]] =
    Operation.readOp {
      v: View =>
        for {
          query <-
            ConstrainedFuture.eitherR(
              for {
                ut <- t.getUnsafe
                left <- instance.lookupTable (ut.leftMostTable)
                right <- instance.lookupTable (ut.rightMostTable)
                pairQuery = CompletedPairQuery (ut, left, right, sd) (instance)
                stringQuery <- pairQuery.render (v)
              } yield stringQuery)

            res <- ConstrainedFuture.either(
              instance
                .reader
                .getObjectPairs[A, B](query)
            )(errors.recoverSQLException)
        } yield res
      }


  override def findDistinct[A](t: FindSingle[A])
                              (
                                implicit extractor: SchemaObject[A],
                                sd: SchemaDescription
                              ): Operation[E, Set[A]] = ???

  override def findDistinctPairs[A, B](t: FindPair[A, B])
                                      (implicit sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription): Operation[E, Set[(A, B)]] = ???

  override def shortestPath[A](start: A, end: A, relationalQuery: RelationalQuery[A, A])
                              (implicit sa: SchemaObject[A], sd: SchemaDescription): Operation[E, Option[Path[A]]] = ???

  override def allShortestPaths[A](start: A, relationalQuery: RelationalQuery[A, A])
                                  (implicit sa: SchemaObject[A], sd: SchemaDescription): Operation[E, Set[Path[A]]] = ???

  override def insert[A, B](t: TraversableOnce[CompletedRelation[A, B]])(
    implicit sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription): Operation[E, Unit] = ???
}