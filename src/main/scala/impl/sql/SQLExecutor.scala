package impl.sql

import core.backend.intermediate.{FindPair, FindSingle}
import core.user.containers.{Operation, Path, ReadOperation, WriteOperation}
import core.user.dsl.{CompletedRelation, E, View}
import core.user.interfaces.DBExecutor
import core.user.schema.SchemaObject
import core.utils.algorithms.PathFinding
import core.utils.{EitherOps, _}
import impl.sql.adt.queries.{CompletedPairQuery, CompletedSingleQuery, PathFindingQuery}
import impl.sql.errors.{SQLError, SQLMissingRelation}
import impl.sql.types.ObjId

import scalaz.Scalaz._
import scalaz._

/**
  * SQL implementation of DBExecutor
  * @param instance - backreference to containing instance
  */

class SQLExecutor(instance: SQLInstance) extends DBExecutor[SQLError] {

  import instance.executionContext

  /**
    * Implement finding a Set of individual values
    * @param t - the query
    * @param sa - Extractor for A
    * @tparam A - type of object to extract
    * @return
    */
  override def find[A](t: FindSingle[A])
                      (
                        implicit sa: SchemaObject[A]
                      ): SQLOperation[Set[A]] =
    new ReadOperation({
      v: View => {
        for {
          // compile query
          query <- compileSingleQuery(t, v)
          // run the SQL
          res <- SQLFutureE(
            instance
              .reader
              .getSingleDistinct[A](query)
          )
        } yield res
      }
  })

  /**
    * Implement finding a Set of related values
    * @param t - the query
    * @param sa - Extractor for A

    * @tparam A - type of object to extract
    * @tparam B - type of object to extract
    *
    * @return
    */
  override def findPairs[A, B](t: FindPair[A, B])
                              (implicit sa: SchemaObject[A], sb: SchemaObject[B]): SQLOperation[Set[(A, B)]] =
    new ReadOperation({
      v: View =>
        for {
        // Compile query to SQL
          query <- compilePairQuery(t, v)
          // Run the query
          res <- SQLFutureE(
            instance
              .reader
              .getDistinctPairs[A, B](query)
          )
        } yield res
  })

  /**
    * Find shortest path between two objects, if one existss
    * @param start - start node
    * @param end - end node
    * @param t - relation with which to find shortest path over
    * @param sa - extractor for A objects
    * @tparam A - type of objects to extract
    * @return
    */

  override def shortestPath[A](start: A, end: A, t: FindPair[A, A])(
    implicit sa: SchemaObject[A]
  ): SQLOperation[Option[Path[A]]] =
    new ReadOperation({
      v: View => {
        // find the table which we want to traverse

        val cfTable = SQLFutureE(
          instance.lookupTable(sa.name)
        )

        // value to start from

        val cfStart = SQLFutureE(
          instance
            .reader
            .getPathfindingEnd(start))

        // find the end object

        val cfEnd = SQLFutureE(
          instance
            .reader
            .getPathfindingEnd(end))

        for {
          s <- cfStart
          e <- cfEnd

          // compile the query to SQL
          query <- compilePathQuery(t, v)

          // get the lookup function
          lookupFn = query.map(instance.reader.getObjIds)

          // get the table
          table <- cfTable

          // find the path
          path <- findPath(s, e, lookupFn)


          // populate the path if it exists
          populatedPath <- SQLFutureE(EitherOps.switch(
            path.map(ids => instance.reader.getPathfindingFound[A](ids, table, v)))
          )
        } yield populatedPath
      }
    })


  /**
    * Find all Paths from the start node
    * @param start - node to start from
    * @param t - relational query to use
    * @param sa - extractor
    * @tparam A - type to extract
    * @return
    */

  override def allShortestPaths[A](start: A, t: FindPair[A, A])
                                  (implicit sa: SchemaObject[A]): SQLOperation[Set[Path[A]]] =
    new ReadOperation({
      v: View => {
        // Get the table to use
        val cfTable = SQLFutureE(
          instance.lookupTable(sa.name)
        )

        // Get the start node
        val cfStart =  SQLFutureE(
          instance
            .reader
            .getPathfindingEnd(start))

        for {
          // compile the query
          query <- compilePathQuery(t, v)

          // get all the pairs related by the query
          // get the lookup function
          lookupFn = query.map(instance.reader.getObjIds)

          // get the table
          table <- cfTable

          s <- cfStart

          // find the path
          paths <- allPaths(s, lookupFn)
          // populate each path
          populatedPaths <- SQLFutureE(EitherOps.sequence(
              paths.map(ids => instance.reader.getPathfindingFound[A](ids, table, v)))
          )
        } yield populatedPaths
      }
  })

  /**
    * Insert a collection of objects to the db
    * @param t - objects to insert
    * @param sa - schema evidence
    * @param sb - schema evidence
    * @tparam A - type to insert
    * @tparam B - type to insert
    * @return
    */
  override def insert[A, B](t: TraversableOnce[CompletedRelation[A, B]])(
    implicit sa: SchemaObject[A], sb: SchemaObject[B]): SQLOperation[Unit] = {
      new WriteOperation (
        view => {
          // set off non-dependent operations asynchronously
          // find the tables
          val cfLeftTable = SQLFutureE(instance.lookupTable(sa.name))
          val cfRightTable = SQLFutureE(instance.lookupTable(sb.name))

          // convert collection of objects into collection of ObjId relations
          val cfSeq =
            SQLFutureE(
              EitherOps.sequence(
                t.map {
                  case CompletedRelation(a, r, b) =>
                    instance.schema.getRelation(r)
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
            // set up a new view, commit
            pair <- instance.writer.setupAndGetNewView(view)
            (newView, newCommit) = pair
            leftTable <- cfLeftTable
            rightTable <- cfRightTable
            processedSeq <- cfSeq
            // write all the relations to the database
            _ <- instance.writer.insertObjects(newView, newCommit, leftTable, rightTable, processedSeq)
          } yield newView
        }
      )
  }

  /**
    * Compile a query used for pathfindinding
    * @param query - Query to compile
    * @param v - view to compile against
    * @tparam A - type of objects
    * @return
    */
  private def compilePathQuery[A](query: FindPair[A, A], v: View): SQLFuture[ObjId => String] =
    SQLFutureE (
      for {
        // get unsafe
        unsafe <- query.getUnsafe(instance.schema).leftMap(SQLMissingRelation)
        // create a query object
        pathQuery = PathFindingQuery(unsafe)(instance)
        // render it
        stringQuery <- pathQuery.getRight(v)
      } yield stringQuery
    )

  /**
    * Compile a query used for extracting pairs
    * @param query - Query to compile
    * @param v - view to compile against
    * @tparam A - type of objects
    * @return
    */
  private def compilePairQuery[A, B](query: FindPair[A, B], v: View): SQLFuture[String] =
    SQLFutureE(
      for {
        // get unsafe
        unsafe <- query.getUnsafe(instance.schema).leftMap(SQLMissingRelation)
        // get left most table
        left <- instance.lookupTable (unsafe.leftMostTable)
        // get right most table
        right <- instance.lookupTable (unsafe.rightMostTable)
        // create a query object
        pairQuery = CompletedPairQuery (unsafe, left, right) (instance)
        // render it
        stringQuery <- pairQuery.render(v)
      } yield stringQuery
    )

  /**
    * Compile a query used for extracting single values
    * @param query - Query to compile
    * @param v - view to compile against
    * @tparam A - type of objects
    * @return
    */
  private def compileSingleQuery[A](query: FindSingle[A], v: View): SQLFuture[String] =
    SQLFutureER(
      for {
        // get erased query
        unsafe <- query.getUnsafe(instance.schema).leftMap(SQLMissingRelation)
        // get the table to lookup from
        table <- instance.lookupTable(unsafe.table)
        // create a query object
        pairQuery = CompletedSingleQuery(unsafe, table) (instance)
        // render it
        stringQuery <- pairQuery.render(v)
      } yield stringQuery
    )

  /**
    * Path find across the given pairs
    * @param s start
    * @param e end
    * @param f subgraph generation function
    * @return
    */
  private def findPath(s: Option[ObjId], e: Option[ObjId], f: ObjId => SQLEither[Set[ObjId]]): SQLFuture[Option[Vector[ObjId]]] =
    SQLFutureE(
      (s, e) match {
        case (None, _) | (_, None) => None.right
        case (Some(start), Some(end)) =>
          for {
            r2 <- PathFinding.singleShortestsPathImpl[SQLError, ObjId](Set(start), end, f)
          } yield r2
      })


  /**
    * Get all paths over a subgraph
    * @param s - starting node
    * @param f - subgraph traversal function
    * @return
    */
  private def allPaths(s: Option[ObjId], f: ObjId => SQLEither[Set[ObjId]]): SQLFuture[Set[Vector[ObjId]]] =
    SQLFutureE {
      s match {
        case None => SQLEither(Set())
        case Some(start) => PathFinding.allShortestPathsImpl(Set(start), f)
      }
  }
}