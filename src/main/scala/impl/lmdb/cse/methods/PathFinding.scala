package impl.lmdb.cse.methods

import core.backend.intermediate.FindPair
import core.user.containers.{Operation, Path, ReadOperation}
import core.user.dsl.{E, ViewId}
import core.user.schema.SchemaObject
import core.utils.{EitherOps, algorithms, _}
import impl.lmdb.common._
import impl.lmdb.common.access.ObjId
import impl.lmdb.common.errors.{LMDBMissingRelation, LMDBMissingTable}

/**
  * Created by Al on 29/12/2017.
  *
  * Highlevel pathfinding implementations
  */

trait PathFinding { self: Methods =>

  import instance.executionContext
  /**
    *
    * @param start - node to start from
    * @param end - target node
    * @param relationalQuery - query to execute over
    * @param sa - evidence that A is something that should be stored in the databases
    * @tparam A - type of objects to extract
    * @return
    */

  def shortestPath[A](
                       start: A,
                       end: A,
                       relationalQuery: FindPair[A, A]
                     )(
    implicit sa: SchemaObject[A]
  ): LMDBOperation[Option[Path[A]]] = new ReadOperation ({
    view: ViewId => LMDBFutureE(
      for {
        // check the view is accessible
        _ <- instance.validateView(view)
        // get the commits associated with the view
        commits <- instance.controlTables.viewsTable.lookupCommits(view)
        // get set of objects matching the target (should only be one)
        initialSet <- instance.lookupPattern(sa.findable(start).getUnsafe, commits)
        // get set of target elements
        targetSet <- instance.lookupPattern(sa.findable(end).getUnsafe, commits)
        target = targetSet.headOption

        // get table to extract results from
        extractorTable <- instance.objects.getOrError(sa.name, LMDBMissingTable(sa.name))

        // get an unsafe equivalent of the query
        query <- relationalQuery.getUnsafe(instance.schema).leftMap(LMDBMissingRelation)

        // set up the search step function
        searchStep = getFrom(query, commits)

        // run a generic pathfinding algorithm
        optionalPath <- target.fold(LMDBEither[Option[Vector[ObjId]]](None)) {
          e => algorithms.PathFinding.singleShortestsPathImpl(initialSet, e, searchStep)
        }

        // extract the path that has been found
        res <- EitherOps.switch(optionalPath.map(p => extractorTable.retrieve[A](p).map(Path.fromVector)))
      } yield res
    )
  })

  /**
    *
    * @param start - node to start from
    * @param relationalQuery - query to execute over
    * @param sa - evidence that A is something that should be stored in the database
    * @tparam A - type of objects to extract
    * @return
    */

  def allShortestPaths[A](
                           start: A,
                           relationalQuery: FindPair[A, A]
                         )(
    implicit sa: SchemaObject[A]
  ): LMDBOperation[Set[Path[A]]] =  new ReadOperation ({
    view: ViewId => LMDBFutureE(
      for {
        // Check the view is accessible
        _ <- instance.validateView(view)

        // get relevant commits
        commits <- instance.controlTables.viewsTable.lookupCommits(view)

        // get the initial set to search from
        initialSet <- instance.lookupPattern(sa.findable(start).getUnsafe, commits)

        // get the table to extract results from
        extractor <- instance.objects.getOrError(sa.name, LMDBMissingTable(sa.name))

        // get the type erased version of the query
        query <- relationalQuery.getUnsafe(instance.schema).leftMap(LMDBMissingRelation)

        // set up the search step function
        searchStep = getFrom(query, commits)

        // find the paths
        paths <- algorithms.PathFinding.allShortestPathsImpl(initialSet, searchStep)

        // extract the paths
        res <- EitherOps.sequence(paths.map(p => extractor.retrieve[A](p).map(Path.fromVector)))
      } yield res
    )
  })
}