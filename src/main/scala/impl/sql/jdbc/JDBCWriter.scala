package impl.sql.jdbc

import core.backend.common.DBObject
import core.user.containers.ConstrainedFuture
import core.user.dsl.View
import core.utils._
import impl.sql._
import impl.sql.tables.{ObjectTable, RelationTable}
import impl.sql.types.{Commit, ObjId}


/**
  * Created by Al on 21/11/2017.
  *
  * A class to handle writes to the PostgreSQL DB via jdbc
  */
class JDBCWriter(implicit instance: SQLInstance) {
  import instance.executionContext
  /**
    * When writing to the SQL database:
    *   We need to
    *   // which order? batched or together
    *
    *     - look up the view current view to get commits
    *     - register a new commit + new view containing the new commit
    *     - insert all of the left hand objects, one by one
    *     - insert all of the right hand objects
    *     - insert all the relations
    */


  /**
    * Creates a new child view + commit of a view
    */
  def setupAndGetNewView(v: View): SQLFuture[(View, Commit)] =
    for {
      connectedCommits <- instance.viewsTable.getCommits(v)
      newView <- instance.viewsRegistry.getNewViewId
      newCommit <- instance.commitsRegistry.getNewcommitId
      _ <- instance.viewsTable.insertNewView(newView, connectedCommits + newCommit)
    } yield (newView, newCommit)


  /**
    * Insert a collection of relations
    * @param view - view to write to
    * @param commit - commit to write to
    * @param leftTable - table to put left elements into
    * @param rightTable - table to put right elements into
    * @param t - collection of relations to insert
    * @return
    */
  def insertObjects(
                     view: View,
                     commit: Commit,
                     leftTable: ObjectTable,
                     rightTable: ObjectTable,
                     t: TraversableOnce[(DBObject, RelationTable, DBObject)]
                   ): SQLFuture[Unit] =
    // insert a bunch of objects
    for {
      // Convert DBObjects to ObjIds
      withLeftIds <- getLeftIds(leftTable, t)
      withRightIds <- getRightIds(rightTable, withLeftIds)
      // find the values that existed beforehand and those that need to be added
      preExisting <- getExistingRelations(withRightIds.mapProj2.toSet, view)
      toAdd = withRightIds.filter {_ notIn preExisting}

      // insert the relations
      res <- insertRelations(toAdd, commit)

      // update the auxialiary tables with newly reachable values
      _ <- consolidateAuxiliaryTables(leftTable, rightTable, toAdd, commit)
    } yield res


  /**
    * Inserts a number of relation pairs
    * @param rels pairs to insert
    * @param commit commit to insert to
    */
  private def insertRelations(
                       rels: List[(ObjId, RelationTable, ObjId)],
                       commit: Commit
                     ): SQLFuture[Unit] = {
    // collect pairs by relation
    val relations = rels.collectSets {case (l, rel, r) => rel -> (l, r)}

    // get convert sets of pairs to a collection of relations
    val queryStrings = Vector.newBuilder[String]

    for {
      (rel, pairs) <- relations
      (l, r) <- pairs
    } queryStrings += rel.insertRelation(l, r, commit)

    // write the queries as a batch
    instance.writeBatch(queryStrings.result())
  }


  /**
    * Convert a traversible of Related pairs to one with the left DBObject substituted for a relevant ObjID,
    * A new one if required
    */
  private def getLeftIds(
                  leftTable: ObjectTable,
                  t: TraversableOnce[(DBObject, RelationTable, DBObject)]
                ): SQLFuture[List[(ObjId, RelationTable, DBObject)]] = {

    // create a local memoised lookup for speed
    val getObj = new Memo(leftTable.insertOrGetObject)
    t.foldRight(SQLFutureI[List[(ObjId, RelationTable, DBObject)]](List())) {
      // for each DBObject, look it up and insert if lookup fails
      case ((lObject, relTable, rObject), cfList) =>
        for {
          lId <- getObj(lObject)
          list <-cfList
        } yield (lId, relTable, rObject) :: list
    }
  }

  /**
    * Convert a traversible of Related pairs to one with the right DBObject substituted for a relevant ObjID,
    * A new one if required
    */
  private def getRightIds(
                  rightTable: ObjectTable,
                  t: List[(ObjId, RelationTable, DBObject)]
                 ): SQLFuture[List[(ObjId, RelationTable, ObjId)]] = {
    // create a local memoised lookup for speed
    val getObject = new Memo(rightTable.insertOrGetObject)
    t.foldRight(SQLFutureI[List[(ObjId, RelationTable, ObjId)]](List())) {
      // for each DBObject, look it up and insert if lookup fails
      case ((lId, relTable, rObject), cfList) =>
        for {
          rId <- getObject(rObject)
          list <- cfList
        } yield (lId, relTable, rId) :: list
    }
  }


  /**
    * Find the relations that already exist for a given view
    * @param t - relation objects to look at
    */
  private def getExistingRelations(
                                    t: Set[RelationTable],
                                    view: View
                          ): SQLFuture[Set[(ObjId, RelationTable, ObjId)]] = {
    // delegate to the relation tables
    val sets = for {
        rel <- t
      } yield rel.getExistingRelations(view).map(_.map{case(l, r) => (l, rel, r)})

      ConstrainedFuture.sequence(sets).map(_.flatten)
    }

  /**
    * Updates the auxiliary table of each of the left and right tables to include the new values
    * @return
    */
  private def consolidateAuxiliaryTables(
                                          leftTable: ObjectTable,
                                          rightTable: ObjectTable,
                                          tuples: List[(ObjId, RelationTable, ObjId)],
                                          commit: Commit
                                        ): SQLFuture[Unit] = SQLFutureE {
    // can optimise if leftTable = rightTable
    if (leftTable == rightTable) {
      val toConsolidate = tuples.foldRight(Set[ObjId]()) {
        case ((left, _, right), s) => (s + left) + right
      }
      // simple delegation
      leftTable.auxTable.insertObjects(toConsolidate, commit)

    } else {
      val toConsolidateLeft = tuples.foldRight(Set[ObjId]()) {
        case ((left, _, _), s) => s + left
      }


      val toConsolidateRight = tuples.foldRight(Set[ObjId]()) {
        case ((_, _, right), s) => s + right
      }

      // chain together delegations
      leftTable
        .auxTable
        .insertObjects(toConsolidateLeft, commit)
        .andThen(
          rightTable
            .auxTable
            .insertObjects(toConsolidateRight, commit)
        )
    }
  }
}


