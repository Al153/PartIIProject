package impl.lmdb.cse.methods

import core.backend.intermediate.FindPair
import core.user.containers.{Operation, ReadOperation}
import core.user.dsl.{E, ViewId}
import core.user.schema.SchemaObject
import core.utils._
import impl.lmdb.common._
import impl.lmdb.common.errors.{LMDBMissingRelation, LMDBMissingTable}


/**
  * Created by Al on 29/12/2017.
  *
  * High level implementation of Pair finding operations
  */

trait FindAllPairs { self: Methods =>
  import instance.executionContext

  /**
    * Straightforward FindDistinctPairs implementation
    * @param t - query to lookup
    * @param sa - evidence of A being something we can put in the database
    * @param sb - evidence of B being something we can put in the databases
    * @tparam A - Type of objects to return
    * @return - A Set of found objects
    */

  def findPairs[A, B](t: FindPair[A, B])(implicit sa: SchemaObject[A], sb: SchemaObject[B]): LMDBOperation[Set[(A, B)]] =
    new ReadOperation({view: ViewId => LMDBFutureE(for {
    // Check the view is accessible
      _ <- instance.controlTables.availableViews.validateView(view)
      // Get the unsafe equivalent of the query
      ut <- t.getUnsafe(instance.schema).leftMap(LMDBMissingRelation)
      // get the tables from which to extract values
      leftTable <- instance.objects.getOrError(sa.name, LMDBMissingTable(sa.name))
      rightTable <- instance.objects.getOrError(sb.name, LMDBMissingTable(sb.name))
      // get the commits needed according to the view
      commits <- instance.controlTables.viewsTable.lookupCommits(view)
      // interpret the ADT
      res <- readSetPairs[A, B](ut, commits, leftTable, rightTable)
    } yield res)})
}

