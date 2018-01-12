package impl.sql

import core.backend.common.algorithms.BreadthFirstTraversal
import core.user.interfaces.DBExecutor
import core.user.containers.{Operation, Path, ReadOperation, WriteOperation}
import core.user.dsl.{CompletedRelation, E, View}
import core.backend.intermediate.{FindPair, FindSingle, RelationalQuery}
import core.user.schema.{SchemaDescription, SchemaObject}
import core.utils.{EitherOps, _}
import impl.sql.adt.queries.{CompletedPairQuery, CompletedSingleQuery, PathFindingQuery}
import impl.sql.errors.SQLMissingRelation
import impl.sql.types.ObjId

/**
  * SQL implementation of DBExecutor
  * @param instance
  */

class SQLExecutor(instance: SQLInstance) extends DBExecutor {

  import instance.executionContext

  /**
    * Implement finding a vector (multiset) of individual values
    * @param t - the query
    * @param sa - Extractor for A
    * @param sd - schema description
    * @tparam A - type of object to extract
    * @return
    */
  override def findAll[A](t: FindSingle[A])
                         (
                           implicit sa: SchemaObject[A],
                           sd: SchemaDescription
                         ): Operation[E, Vector[A]] =
    new ReadOperation({
      v: View =>
        (for {
          // compile the query into SQL
          query <- compileSingleQuery(t, sd, v)
          // run the SQL
          res <- SQLFutureE(
            instance
              .reader
              .getAllSingles[A](query)
          )
        } yield res).asCFuture
    })

  /**
    * Implement finding a Vector (multiset) of related values
    * @param t - the query
    * @param sa - Extractor for A
    * @param sd - schema description
    * @tparam A - type of object to extract
    * @tparam B - type of object to extract
    *
    * @return
    */
  override def findAllPairs[A, B](t: FindPair[A, B])
                                 (
                                   implicit sa: SchemaObject[A],
                                   sb: SchemaObject[B],
                                   sd: SchemaDescription
                                 ): Operation[E, Vector[(A, B)]] =
    new ReadOperation({
      v: View =>
        (for {
          // compile the query
          query <- compilePairQuery(t, sd, v)
          // run the compiled SQL query
          res <- SQLFutureE(
            instance
              .reader
              .getAllPairs[A, B](query)
          )
        } yield res).asCFuture
    })

  /**
    * Implement finding a Set of individual values
    * @param t - the query
    * @param sa - Extractor for A
    * @param sd - schema description
    * @tparam A - type of object to extract
    * @return
    */
  override def findDistinct[A](t: FindSingle[A])
                              (
                                implicit sa: SchemaObject[A],
                                sd: SchemaDescription
                              ): Operation[E, Set[A]] =
    new ReadOperation({
      v: View => {
        for {
          // compile query
          query <- compileSingleQuery(t, sd, v)
          // run the SQL
          res <- SQLFutureE(
            instance
              .reader
              .getSingleDistinct[A](query)
          )
        } yield res
      }.asCFuture
  })

  /**
    * Implement finding a Set of related values
    * @param t - the query
    * @param sa - Extractor for A
    * @param sd - schema description
    * @tparam A - type of object to extract
    * @tparam B - type of object to extract
    *
    * @return
    */
  override def findDistinctPairs[A, B](t: FindPair[A, B])
                                      (implicit sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription): Operation[E, Set[(A, B)]] =
    new ReadOperation({
      v: View =>
        (for {
          // Compile query to SQL
          query <- compilePairQuery(t, sd, v)
          // Run the query
          res <- SQLFutureE(
            instance
              .reader
              .getDistinctPairs[A, B](query)
          )
        } yield res).asCFuture
  })

  /**
    * Find shortest path between two objects, if one existss
    * @param start - start node
    * @param end - end node
    * @param t - relation with which to find shortest path over
    * @param sa - extractor for A objects
    * @param sd
    * @tparam A - type of objects to extract
    * @return
    */

  override def shortestPath[A](start: A, end: A, t: RelationalQuery[A, A])
                              (
                                implicit sa: SchemaObject[A],
                                sd: SchemaDescription
                              ): Operation[E, Option[Path[A]]] =
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
          query <- compilePathQuery(t.tree(sd), sd, v)

          // Find all pairs related by the relation
          pairs <- SQLFutureE(
            instance
              .reader
              .getRelationPairs(query))


          table <- cfTable

          // find the path
          path <- findPath(s, e, pairs)


          // populate the path if it exists
          populatedPath <- SQLFutureE(EitherOps.switch(
            path.map(ids => instance.reader.getPathfindingFound[A](ids, table, v)))
          )
        } yield populatedPath
      }.asCFuture
    })


  /**
    * Find all Paths from the start node
    * @param start - node to start from
    * @param t - relational query to use
    * @param sa - extractor
    * @param sd - schema description
    * @tparam A - type to extract
    * @return
    */

  override def allShortestPaths[A](start: A, t: RelationalQuery[A, A])
                                  (
                                    implicit sa: SchemaObject[A],
                                    sd: SchemaDescription
                                  ): Operation[E, Set[Path[A]]] =
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
          query <- compilePathQuery(t.tree(sd), sd, v)

          // get all the pairs related by the query
          pairs <- SQLFutureE(
            instance
              .reader
              .getRelationPairs(query))


          s <- cfStart
          table <- cfTable

          // run the path finding algorithm
          paths <- allPaths(s, pairs)

          // populate each path
          populatedPaths <- SQLFutureE(EitherOps.sequence(
              paths.map(ids => instance.reader.getPathfindingFound[A](ids, table, v)))
          )
        } yield populatedPaths
      }.asCFuture
  })

  /**
    * Insert a collection of objects to the db
    * @param t - objects to insert
    * @param sa - schema evidence
    * @param sb - schema evidence
    * @param sd
    * @tparam A - type to insert
    * @tparam B - type to insert
    * @return
    */
  override def insert[A, B](t: TraversableOnce[CompletedRelation[A, B]])(
    implicit sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription): Operation[E, Unit] = {
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
            // set up a new view, commit
            pair <- instance.writer.setupAndGetNewView(view)
            (newView, newCommit) = pair
            leftTable <- cfLeftTable
            rightTable <- cfRightTable
            processedSeq <- cfSeq
            // write all the relations to the database
            _ <- instance.writer.insertObjects(newView, newCommit, leftTable, rightTable, processedSeq)
          } yield newView
        }.asCFuture
      )
  }

  /**
    * Compile a query used for pathfindinding
    * @param query - Query to compile
    * @param v - view to compile against
    * @tparam A - type of objects
    * @return
    */
  private def compilePathQuery[A](query: FindPair[A, A], schemaDescription: SchemaDescription, v: View): SQLFuture[String] =
    SQLFutureE (
      for {
        // get unsafe
        unsafe <- query.getUnsafe.leftMap(SQLMissingRelation)
        // create a query object
        pathQuery = PathFindingQuery(unsafe)(instance)
        // render it
        stringQuery <- pathQuery.render(v)
      } yield stringQuery
    )

  /**
    * Compile a query used for extracting pairs
    * @param query - Query to compile
    * @param v - view to compile against
    * @tparam A - type of objects
    * @return
    */
  private def compilePairQuery[A, B](query: FindPair[A, B], sd: SchemaDescription, v: View): SQLFuture[String] =
    SQLFutureE(
      for {
        // get unsafe
        unsafe <- query.getUnsafe.leftMap(SQLMissingRelation)
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
  private def compileSingleQuery[A](query: FindSingle[A], sd: SchemaDescription, v: View): SQLFuture[String] =
    SQLFutureER(
      for {
        // get erased query
        unsafe <- query.getUnsafe.leftMap(SQLMissingRelation)
        // get the table to lookup from
        table <- instance.lookupTable(unsafe.table)
        // create a query object
        pairQuery = CompletedSingleQuery(unsafe, table, sd) (instance)
        // render it
        stringQuery <- pairQuery.render(v)
      } yield stringQuery
    )

  /**
    * Path find across the given pairs
    * @param s start
    * @param e end
    * @param pairs pairs which give the subgraph to search
    * @return
    */
  private def findPath(s: Option[ObjId], e: Option[ObjId], pairs: Set[(ObjId, ObjId)]): SQLFuture[Option[Vector[ObjId]]] =
    SQLFuture {
      for {
        s <- s
        e <- e
        // create an index
        index = pairs.collectSets(identity)
        // use a generic pathfinding algorithm
        res <- BreadthFirstTraversal.breadthFirstSearch[ObjId](s, k => index.getOrElse(k, Set()), e)
      } yield res
    }

  /**
    * Get all paths over a subgraph
    * @param s - starting node
    * @param pairs - subgraph
    * @return
    */
  private def allPaths(s: Option[ObjId], pairs: Set[(ObjId, ObjId)]): SQLFuture[Set[Vector[ObjId]]] =
    SQLFuture {
      val index = pairs.collectSets(identity)
      s match {
        case None => Set()
        case Some(start) => BreadthFirstTraversal.allPaths(start, k => index.getOrElse(k, Set()))
      }
  }
}