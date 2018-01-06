package impl.lmdb.methods

import core.user.containers.{Operation, ReadOperation}
import core.user.dsl.{E, View}
import core.backend.intermediate.FindPair
import core.user.schema.{SchemaDescription, SchemaObject}
import impl.lmdb.LMDBFutureE
import impl.lmdb.errors.{LMDBMissingRelation, LMDBMissingTable}
import core.utils._

/**
  * Created by Al on 29/12/2017.
  */

trait FindAllPairs { self: Methods =>

  import instance._

  def findAllPairs[A, B](
                          t: FindPair[A, B]
                        )(
    implicit sa: SchemaObject[A],
    sb: SchemaObject[B],
    sd: SchemaDescription
  ): Operation[E, Vector[(A, B)]] = new ReadOperation({view: View => LMDBFutureE(for {
    _ <- instance.controlTables.availableViews.validateView(view)
    ut <- t.getUnsafe.leftMap(LMDBMissingRelation)
    leftTable <- instance.objects.getOrError(sa.tableName, LMDBMissingTable(sa.tableName))
    rightTable <- instance.objects.getOrError(sb.tableName, LMDBMissingTable(sb.tableName))
    commits <- instance.controlTables.viewsTable.lookupCommits(view)
    res <- findVectorPairs[A, B](ut, commits, leftTable, rightTable)
  } yield res).asCFuture})


  def findDistinctPairs[A, B](t: FindPair[A, B])(implicit sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription): Operation[E, Set[(A, B)]] =
    new ReadOperation({view: View => LMDBFutureE(for {
      _ <- instance.controlTables.availableViews.validateView(view)
      ut <- t.getUnsafe.leftMap(LMDBMissingRelation)
      leftTable <- instance.objects.getOrError(sa.tableName, LMDBMissingTable(sa.tableName))
      rightTable <- instance.objects.getOrError(sb.tableName, LMDBMissingTable(sb.tableName))
      commits <- instance.controlTables.viewsTable.lookupCommits(view)
      res <- readSetPairs[A, B](ut, commits, leftTable, rightTable)
    } yield res).asCFuture})
}

