package impl.sql

import core.backend.interfaces.DBExecutor
import core.containers.{ConstrainedFuture, Operation, Path}
import core.dsl.RelationalQuery
import core.error.E
import core.intermediate.{FindPair, FindSingle}
import core.relations.CompletedRelation
import core.schema.{SchemaDescription, SchemaObject}
import core.utils.EitherOps
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
          query <- compileSingleQuery(t, sd, v)
          res <- ConstrainedFuture.either(
            instance
              .reader
              .getAllSingles[A](query)
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
          query <- compilePairQuery(t, sd, v)
            res <- ConstrainedFuture.either(
              instance
                .reader
                .getAllPairs[A, B](query)
            )(errors.recoverSQLException)
        } yield res
      }


  override def findDistinct[A](t: FindSingle[A])
                              (
                                implicit extractor: SchemaObject[A],
                                sd: SchemaDescription
                              ): Operation[E, Set[A]] =     Operation.readOp {
    v: View =>
      for {
        query <- compileSingleQuery(t, sd, v)
        res <- ConstrainedFuture.either(
          instance
            .reader
            .getSingleDistinct[A](query)
        )(errors.recoverSQLException)
      } yield res
  }

  override def findDistinctPairs[A, B](t: FindPair[A, B])
                                      (implicit sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription): Operation[E, Set[(A, B)]] =     Operation.readOp {
    v: View =>
      for {
        query <- compilePairQuery(t, sd, v)
        res <- ConstrainedFuture.either(
          instance
            .reader
            .getDistinctPairs[A, B](query)
        )(errors.recoverSQLException)
      } yield res
  }

  // Todo: this needs a separate stack of code for gettingobjects WITH ID so we can retroactively find objects in the set

  override def shortestPath[A](start: A, end: A, t: RelationalQuery[A, A])
                              (
                                implicit sa: SchemaObject[A],
                                sd: SchemaDescription
                              ): Operation[E, Option[Path[A]]] = Operation.readOp {
    v: View => {
      val pairs =
        for {
          query <- compilePairQuery(t.tree(sd), sd, v)
          res <- ConstrainedFuture.either(
            instance
              .reader
              .getDistinctPairs[A, A](query)
          )(errors.recoverSQLException)
        } yield res
      ???
    }
  }





  override def allShortestPaths[A](start: A, t: RelationalQuery[A, A])
                                  (
                                    implicit sa: SchemaObject[A],
                                    sd: SchemaDescription
                                  ): Operation[E, Set[Path[A]]] = Operation.readOp {
    v: View => {
      val pairs =
        for {
          query <- compilePairQuery(t.tree(sd), sd, v)
          res <- ConstrainedFuture.either(
            instance
              .reader
              .getDistinctPairs[A, A](query)
          )(errors.recoverSQLException)
        } yield res
      ???
    }
  }

  override def insert[A, B](t: TraversableOnce[CompletedRelation[A, B]])(
    implicit sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription): Operation[E, Unit] = {
      Operation.writeOp(
        view =>
          for {
            pair <- instance.writer.setupAndGetNewView(view)
            (newView, newCommit) = pair
            leftTable <- ConstrainedFuture.eitherR(instance.lookupTable(sa.tableName))
            rightTable <- ConstrainedFuture.eitherR(instance.lookupTable(sb.tableName))
            processedSeq <-
              ConstrainedFuture.eitherR(
                EitherOps.sequence(
                  t.map {
                    case CompletedRelation(a, r, b) =>
                      sd.getRelation(r).flatMap(
                        r =>
                          instance
                            .lookupRelation(r)
                            .map(
                              r => (sa.getDBObject(a), r, sb.getDBObject(b))
                            )
                      )

                  }
                )
              )
            _ <- instance.writer.insertObjects(newView, newCommit, leftTable, rightTable, processedSeq)
          } yield newView
      )
  }


 private def compilePairQuery[A, B](query: FindPair[A, B], sd: SchemaDescription, v: View): ConstrainedFuture[E, String] =
   ConstrainedFuture.eitherR(
     for {
       unsafe <- query.getUnsafe
       left <- instance.lookupTable (unsafe.leftMostTable)
       right <- instance.lookupTable (unsafe.rightMostTable)
       pairQuery = CompletedPairQuery (unsafe, left, right, sd) (instance)
       stringQuery <- pairQuery.render(v)
     } yield stringQuery)

  private def compileSingleQuery[A](query: FindSingle[A], sd: SchemaDescription, v: View): ConstrainedFuture[E, String] =
    ConstrainedFuture.eitherR(
      for {
        unsafe <- query.getUnsafe
        table <- instance.lookupTable (unsafe.table)
        pairQuery = CompletedSingleQuery(unsafe, table, sd) (instance)
        stringQuery <- pairQuery.render (v)
      } yield stringQuery
    )
}