package impl.lmdb.methods

import core.backend.common.algorithms
import core.containers.{Operation, Path, ReadOperation}
import core.dsl.RelationalQuery
import core.error.E
import core.schema.{SchemaDescription, SchemaObject}
import core.utils.{EitherOps, _}
import core.view.View
import impl.lmdb._
import impl.lmdb.access.ObjId
import impl.lmdb.errors.{LMDBEmptyFringe, LMDBMissingRelation, LMDBMissingTable}

/**
  * Created by Al on 29/12/2017.
  */

trait PathFinding { self: Methods =>
  import instance._

  def shortestPath[A](
                       start: A,
                       end: A,
                       relationalQuery: RelationalQuery[A, A]
                     )(
    implicit sa: SchemaObject[A],
    sd: SchemaDescription
  ): Operation[E, Option[Path[A]]] = new ReadOperation ({
    view: View => LMDBFutureE(
      for {
        _ <- instance.validateView(view)
        commits <- instance.controlTables.viewsTable.lookupCommits(view)
        initialSet <- instance.lookupPattern(sa.findable(start).getUnsafe, commits)
        targetSet <- instance.lookupPattern(sa.findable(end).getUnsafe, commits)
        target = targetSet.find(_ => true)

        extractor <- instance.objects.getOrError(sa.tableName, LMDBMissingTable(sa.tableName))

        query <- relationalQuery.tree.getUnsafe.leftMap(LMDBMissingRelation)
        searchStep = {o: ObjId => findPairSet(query, commits,  Set(o))}

        optionalPath <- target.fold(LMDBEither[Option[Vector[ObjId]]](None)) {
          e => algorithms.PathFinding.singleShortestsPathImpl(initialSet, e, searchStep, LMDBEmptyFringe)
        }

        res <- EitherOps.switch(optionalPath.map(p => extractor.retrieve[A](p).map(Path.fromVector)))
      } yield res
    ).asCFuture
  })


  def allShortestPaths[A](
                           start: A,
                           relationalQuery: RelationalQuery[A, A]
                         )(
    implicit sa: SchemaObject[A],
    sd: SchemaDescription
  ): Operation[E, Set[Path[A]]] =  new ReadOperation ({
    view: View => LMDBFutureE(
      for {
        _ <- instance.validateView(view)
        commits <- instance.controlTables.viewsTable.lookupCommits(view)
        initialSet <- instance.lookupPattern(sa.findable(start).getUnsafe, commits)

        extractor <- instance.objects.getOrError(sa.tableName, LMDBMissingTable(sa.tableName))

        query <- relationalQuery.tree.getUnsafe.leftMap(LMDBMissingRelation)
        searchStep = {o: ObjId => findPairSet(query, commits,  Set(o))}

        optionalPath <- algorithms.PathFinding.allShortestPathsImpl(initialSet, searchStep, LMDBEmptyFringe)


        res <- EitherOps.sequence(optionalPath.map(p => extractor.retrieve[A](p).map(Path.fromVector)))
      } yield res
    ).asCFuture
  })
}