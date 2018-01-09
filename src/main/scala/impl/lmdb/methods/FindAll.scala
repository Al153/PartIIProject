package impl.lmdb.methods

import core.user.containers.{Operation, ReadOperation}
import core.user.dsl.{E, View}
import core.backend.intermediate.FindSingle
import core.user.schema.{SchemaDescription, SchemaObject}
import core.utils._
import impl.lmdb._
import impl.lmdb.errors.{LMDBMissingRelation, LMDBMissingTable}

/**
  * Created by Al on 29/12/2017.
  */

trait FindAll { self: Methods =>
  import instance._

  def findAll[A](t: FindSingle[A])(implicit extractor: SchemaObject[A], sd: SchemaDescription): Operation[E, Vector[A]] =
    new ReadOperation({view: View => LMDBFutureE(for {
      _ <- instance.controlTables.availableViews.validateView(view)
      ut <- t.getUnsafe.leftMap(LMDBMissingRelation)
      objectTable <- instance.objects.getOrError(extractor.name, LMDBMissingTable(extractor.name))
      commits <- instance.controlTables.viewsTable.lookupCommits(view)
      res <- findVector(ut, commits, objectTable)
    } yield res).asCFuture})


  def findDistinct[A](t: FindSingle[A])(implicit extractor: SchemaObject[A], sd: SchemaDescription): Operation[E, Set[A]] =
    new ReadOperation({view: View => LMDBFutureE(for {
      _ <- instance.controlTables.availableViews.validateView(view)
      ut <- t.getUnsafe.leftMap(LMDBMissingRelation)
      objectTable <- instance.objects.getOrError(extractor.name, LMDBMissingTable(extractor.name))
      commits <- instance.controlTables.viewsTable.lookupCommits(view)
      res <- findVector(ut, commits, objectTable)
    } yield res.toSet).asCFuture})


}