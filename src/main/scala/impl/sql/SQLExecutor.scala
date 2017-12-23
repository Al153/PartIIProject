package impl.sql

import core.backend.common.Algorithms
import core.backend.interfaces.DBExecutor
import core.containers.{Operation, Path}
import core.dsl.RelationalQuery
import core.error.E
import core.intermediate.{FindPair, FindSingle}
import core.relations.CompletedRelation
import core.schema.{SchemaDescription, SchemaObject}
import core.utils.{EitherOps, _}
import core.view.View
import impl.sql.adt.queries.{CompletedPairQuery, CompletedSingleQuery, PathFindingQuery}
import impl.sql.errors.SQLMissingRelation
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
        (for {
          query <- compileSingleQuery(t, sd, v)
          res <- SQLFutureE(
            instance
              .reader
              .getAllSingles[A](query)
          )
        } yield res).asCFuture
    }

  override def findAllPairs[A, B](t: FindPair[A, B])
                                 (
                                   implicit sa: SchemaObject[A],
                                   sb: SchemaObject[B],
                                   sd: SchemaDescription
                                 ): Operation[E, Vector[(A, B)]] =
    Operation.readOp {
      v: View =>
        (for {
          query <- compilePairQuery(t, sd, v)
            res <- SQLFutureE(
              instance
                .reader
                .getAllPairs[A, B](query)
            )
        } yield res).asCFuture
      }


  override def findDistinct[A](t: FindSingle[A])
                              (
                                implicit extractor: SchemaObject[A],
                                sd: SchemaDescription
                              ): Operation[E, Set[A]] =
    Operation.readOp {
      v: View => {
        for {
          query <- compileSingleQuery(t, sd, v)
          res <- SQLFutureE(
            instance
              .reader
              .getSingleDistinct[A](query)
          )
        } yield res
      }.asCFuture
  }

  override def findDistinctPairs[A, B](t: FindPair[A, B])
                                      (implicit sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription): Operation[E, Set[(A, B)]] =
    Operation.readOp {
      v: View =>
        (for {
          query <- compilePairQuery(t, sd, v)
          res <- SQLFutureE(
            instance
              .reader
              .getDistinctPairs[A, B](query)
          )
        } yield res).asCFuture
  }

  override def shortestPath[A](start: A, end: A, t: RelationalQuery[A, A])
                              (
                                implicit sa: SchemaObject[A],
                                sd: SchemaDescription
                              ): Operation[E, Option[Path[A]]] = Operation.readOp {
    v: View => {
      val cfTable = SQLFutureE(
        instance.lookupTable(sa.tableName)
      )

      val cfStart = SQLFutureE(
        instance
          .reader
          .getPathfindingEnd(start))

      val cfEnd = SQLFutureE(
        instance
          .reader
          .getPathfindingEnd(end))

      for {
        s <- cfStart
        e <- cfEnd

        _ = println("Start = " + s)
        _ = println("end = " + s)

        query <- compilePathQuery(t.tree(sd), sd, v)

      _ = println("path query = (" +  query + ")")

        pairs <- SQLFutureE(
          instance
            .reader
            .getRelationPairs(query))

        _ = println(pairs)

        table <- cfTable

        path <- findPath(s, e, pairs)
        _ = println(path)
        populatedPath <- SQLFutureE(EitherOps.switch(
          path.map(ids => instance.reader.getPathfindingFound[A](ids, table, v)))
        )
      } yield populatedPath
    }.asCFuture
  }





  override def allShortestPaths[A](start: A, t: RelationalQuery[A, A])
                                  (
                                    implicit sa: SchemaObject[A],
                                    sd: SchemaDescription
                                  ): Operation[E, Set[Path[A]]] = Operation.readOp {
    v: View => {
      val cfTable = SQLFutureE(
        instance.lookupTable(sa.tableName)
      )

      val cfStart =  SQLFutureE(
        instance
          .reader
          .getPathfindingEnd(start))

      for {
        query <- compilePathQuery(t.tree(sd), sd, v)
        pairs <- SQLFutureE(
          instance
            .reader
            .getRelationPairs(query))

        s <- cfStart
        table <- cfTable

        paths <- allPaths(s, pairs)
        populatedPaths <- SQLFutureE(EitherOps.sequence(
            paths.map(ids => instance.reader.getPathfindingFound[A](ids, table, v)))
        )
      } yield populatedPaths
    }.asCFuture
  }

  override def insert[A, B](t: TraversableOnce[CompletedRelation[A, B]])(
    implicit sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription): Operation[E, Unit] = {
      Operation.writeOp(
        view => {
          // set off non-dependent operations asynchronously
          val cfLeftTable = SQLFutureE(instance.lookupTable(sa.tableName))
          val cfRightTable = SQLFutureE(instance.lookupTable(sb.tableName))
          val cfSeq =
            SQLFutureE(
              EitherOps.sequence(
                t.map {
                  case CompletedRelation(a, r, b) =>
                    sd.getRelation(r)
                      .leftMap(SQLMissingRelation)
                      .flatMap(
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
        }.asCFuture
      )
  }

  private def compilePathQuery[A](query: FindPair[A, A], schemaDescription: SchemaDescription, v: View): SQLFuture[String] =
    SQLFutureE (
      for {
        unsafe <- query.getUnsafe.leftMap(SQLMissingRelation)
        pathQuery = PathFindingQuery(unsafe)(instance)
        stringQuery <- pathQuery.render(v)
      } yield stringQuery
    )

  private def compilePairQuery[A, B](query: FindPair[A, B], sd: SchemaDescription, v: View): SQLFuture[String] =
    SQLFutureE(
      for {
        unsafe <- query.getUnsafe.leftMap(SQLMissingRelation)
        left <- instance.lookupTable (unsafe.leftMostTable)
        right <- instance.lookupTable (unsafe.rightMostTable)
        pairQuery = CompletedPairQuery (unsafe, left, right) (instance)
        stringQuery <- pairQuery.render(v)
      } yield stringQuery
    )

  private def compileSingleQuery[A](query: FindSingle[A], sd: SchemaDescription, v: View): SQLFuture[String] =
    SQLFutureER(
      for {
        unsafe <- query.getUnsafe.leftMap(SQLMissingRelation)
        table <- instance.lookupTable(unsafe.table)
        pairQuery = CompletedSingleQuery(unsafe, table, sd) (instance)
        stringQuery <- pairQuery.render(v)
      } yield stringQuery
    )

  private def findPath(s: Option[ObjId], e: Option[ObjId], pairs: Set[(ObjId, ObjId)]): SQLFuture[Option[List[ObjId]]] =
    SQLFuture {
      for {
        s <- s
        e <- e
        index = pairs.collectSets(identity)
        res <- Algorithms.breadthFirstSearch[ObjId](s, k => index.getOrElse(k, Set()), e)
      } yield res
    }

  private def allPaths(s: Option[ObjId], pairs: Set[(ObjId, ObjId)]): SQLFuture[Set[List[ObjId]]] =
    SQLFuture {
      val index = pairs.collectSets(identity)
      s match {
        case None => Set()
        case Some(start) => Algorithms.allPaths(start, k => index.getOrElse(k, Set()))
      }
  }
}