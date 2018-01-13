package impl.lmdb.methods

import core.user.containers.{Operation, ReadOperation}
import core.user.dsl.{E, View}
import core.backend.intermediate.FindSingle
import core.user.schema.SchemaObject
import core.utils._
import impl.lmdb._
import impl.lmdb.errors.{LMDBMissingRelation, LMDBMissingTable}

/**
  * Created by Al on 29/12/2017.
  *
  * Top-level implementation of the find single operations
  */

trait FindAll { self: Methods =>
  import instance._

  /**
    * Straight forward findSingle implementation
    * @param t - query to lookup
    * @param sa - evidence of A being something we can put in the database
    * @tparam A - Type of objects to return
    * @return - A Vector (multiset) of found objects
    */
  def findAll[A](t: FindSingle[A])(implicit sa: SchemaObject[A]): Operation[E, Vector[A]] =
    new ReadOperation({view: View => LMDBFutureE(for {
      // Check the view is valid
      _ <- instance.controlTables.availableViews.validateView(view)
      // to type erasure on the findSingle
      ut <- t.getUnsafe(instance.schema).leftMap(LMDBMissingRelation)
      // get hold of the object table to retrieve results from
      objectTable <- instance.objects.getOrError(sa.name, LMDBMissingTable(sa.name))
      // Get the commits to use
      commits <- instance.controlTables.viewsTable.lookupCommits(view)
      // Interpret the ADT
      res <- readVector(ut, commits, objectTable)
    } yield res).asCFuture})

  /**
    * Straight forward findDisinct implementation
    * @param t - query to lookup
    * @param sa - evidence of A being something we can put in the database
    * @tparam A - Type of objects to return
    * @return - A Set of found objects
    */
  def findDistinct[A](t: FindSingle[A])(implicit sa: SchemaObject[A]): Operation[E, Set[A]] =
    new ReadOperation({view: View => LMDBFutureE(for {
    // Check the view is valid
      _ <- instance.controlTables.availableViews.validateView(view)
      // to type erasure on the findSingle
      ut <- t.getUnsafe(instance.schema).leftMap(LMDBMissingRelation)
      // get hold of the object table to retrieve results from
      objectTable <- instance.objects.getOrError(sa.name, LMDBMissingTable(sa.name))
      // Get the commits to use
      commits <- instance.controlTables.viewsTable.lookupCommits(view)
      // Interpret the ADT
      res <- readSet(ut, commits, objectTable)
    } yield res).asCFuture})


}