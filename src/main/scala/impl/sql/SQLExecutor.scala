package impl.sql

import core.backend.common.Algorithms
import core.backend.interfaces.DBExecutor
import core.containers.{ConstrainedFuture, Operation, Path}
import core.dsl.RelationalQuery
import core.error.E
import core.intermediate.{FindPair, FindSingle}
import core.relations.CompletedRelation
import core.schema.{SchemaDescription, SchemaObject}
import core.utils.{EitherOps, _}
import core.view.View
import impl.sql.adt.queries.{CompletedPairQuery, CompletedSingleQuery, PathFindingQuery}
import impl.sql.types.ObjId

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

  // Todo: this needs a separate stack of code for getting objects WITH ID so we can retroactively find objects in the set

  override def shortestPath[A](start: A, end: A, t: RelationalQuery[A, A])
                              (
                                implicit sa: SchemaObject[A],
                                sd: SchemaDescription
                              ): Operation[E, Option[Path[A]]] = Operation.readOp {
    v: View => {
      val cfTable = ConstrainedFuture.either(
        instance.lookupTable(sa.tableName)
      )(errors.recoverSQLException)

      val cfStart = ConstrainedFuture.either(
        instance
          .reader
          .getPathfindingEnd(start))(errors.recoverSQLException)

      val cfEnd = ConstrainedFuture.either(
        instance
          .reader
          .getPathfindingEnd(end))(errors.recoverSQLException)

      for {
        s <- cfStart
        e <- cfEnd

        query <- compilePathQuery(t.tree(sd), sd, v)
        pairs <- ConstrainedFuture.either(
          instance
            .reader
            .getPathfindingPairs(query))(errors.recoverSQLException)


        table <- cfTable

        path <- findPath(s, e, pairs)
        populatedPath <- ConstrainedFuture.either(EitherOps.switch(
          path.map(ids => instance.reader.getPathfindingFound[A](ids, table, v)))
        )(errors.recoverSQLException)
      } yield populatedPath
    }
  }





  override def allShortestPaths[A](start: A, t: RelationalQuery[A, A])
                                  (
                                    implicit sa: SchemaObject[A],
                                    sd: SchemaDescription
                                  ): Operation[E, Set[Path[A]]] = Operation.readOp {
    v: View => {
      val cfTable = ConstrainedFuture.either(
        instance.lookupTable(sa.tableName)
      )(errors.recoverSQLException)

      val cfStart =  ConstrainedFuture.either(
        instance
          .reader
          .getPathfindingEnd(start))(errors.recoverSQLException)

      for {
        query <- compilePathQuery(t.tree(sd), sd, v)
        pairs <- ConstrainedFuture.either(
          instance
            .reader
            .getPathfindingPairs(query))(errors.recoverSQLException)

        s <- cfStart
        table <- cfTable

        paths <- allPaths(s, pairs)
        populatedPaths <- ConstrainedFuture.either(EitherOps.sequence(
            paths.map(ids => instance.reader.getPathfindingFound[A](ids, table, v)))
        )(errors.recoverSQLException)
      } yield populatedPaths
    }
  }

  override def insert[A, B](t: TraversableOnce[CompletedRelation[A, B]])(
    implicit sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription): Operation[E, Unit] = {
      Operation.writeOp(
        view => {
          // set off non-dependent operations asynchronously
          val cfLeftTable = ConstrainedFuture.eitherR(instance.lookupTable(sa.tableName))
          val cfRightTable = ConstrainedFuture.eitherR(instance.lookupTable(sb.tableName))
          val cfSeq =
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

          for {
            pair <- instance.writer.setupAndGetNewView(view)
            (newView, newCommit) = pair
            leftTable <- cfLeftTable
            rightTable <- cfRightTable
            processedSeq <- cfSeq
            _ <- instance.writer.insertObjects(newView, newCommit, leftTable, rightTable, processedSeq)
          } yield newView
        }

      )
  }

  private def compilePathQuery[A](query: FindPair[A, A], schemaDescription: SchemaDescription, v: View): ConstrainedFuture[E, String] =
    ConstrainedFuture.eitherR(
      for {
        unsafe <- query.getUnsafe
        pathQuery = PathFindingQuery(unsafe)(instance)
        stringQuery <- pathQuery.render(v)
      } yield stringQuery
    )

  private def compilePairQuery[A, B](query: FindPair[A, B], sd: SchemaDescription, v: View): ConstrainedFuture[E, String] =
    ConstrainedFuture.eitherR(
      for {
        unsafe <- query.getUnsafe
        left <- instance.lookupTable (unsafe.leftMostTable)
        right <- instance.lookupTable (unsafe.rightMostTable)
        pairQuery = CompletedPairQuery (unsafe, left, right) (instance)
        stringQuery <- pairQuery.render(v)
      } yield stringQuery
    )

  private def compileSingleQuery[A](query: FindSingle[A], sd: SchemaDescription, v: View): ConstrainedFuture[E, String] =
    ConstrainedFuture.eitherR(
      for {
        unsafe <- query.getUnsafe
        table <- instance.lookupTable (unsafe.table)
        pairQuery = CompletedSingleQuery(unsafe, table, sd) (instance)
        stringQuery <- pairQuery.render (v)
      } yield stringQuery
    )

  private def findPath(s: ObjId, e: ObjId, pairs: Set[(ObjId, ObjId)]): ConstrainedFuture[E, Option[List[ObjId]]] =
    ConstrainedFuture.point[E, Option[List[ObjId]]] {
      val index = pairs.collectSets(identity)
      Algorithms.breadthFirstSearch[ObjId](s, k => index.getOrElse(k, Set()), e)
    }(errors.recoverSQLException)

  private def allPaths(s: ObjId, pairs: Set[(ObjId, ObjId)]): ConstrainedFuture[E, Set[List[ObjId]]] =
    ConstrainedFuture.point[E, Set[List[ObjId]]] {
      val index = pairs.collectSets(identity)
      Algorithms.allPaths(s, k => index.getOrElse(k, Set()))
  }(errors.recoverSQLException)


}