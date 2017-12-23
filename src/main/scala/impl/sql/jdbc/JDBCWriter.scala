package impl.sql.jdbc

import core.backend.common.DBObject
import core.containers.ConstrainedFuture
import core.utils._
import core.view.View
import impl.sql._
import impl.sql.tables.{ObjectTable, RelationTable}
import impl.sql.types.{Commit, ObjId}


/**
  * Created by Al on 21/11/2017.
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


  // this method needs to setup a new view
  def setupAndGetNewView(v: View): SQLFuture[(View, Commit)] =
    for {
      connectedCommits <- instance.viewsTable.getCommits(v)
      newView <- instance.viewsRegistry.getNewViewId
      newCommit <- instance.commitsRegistry.getNewcommitId
      _ <- instance.viewsTable.insertNewView(newView, connectedCommits + newCommit)
    } yield (newView, newCommit)


  def insertObjects(
                     view: View,
                     commit: Commit,
                     leftTable: ObjectTable,
                     rightTable: ObjectTable,
                     t: TraversableOnce[(DBObject, RelationTable, DBObject)]
                   ): SQLFuture[Unit] =
    // insert a bunch of objects
    for {
      withLeftIds <- getLeftIds(leftTable, t)
      withRightIds <- getRightIds(rightTable, withLeftIds)
      preExisting <- getExistingRelations(withRightIds.mapProj2.toSet, view) // todo: we need to join each table with this
      toAdd = withRightIds.filter {_ notIn preExisting}
      res <- insertRelations(toAdd, commit)
    } yield res


  private def insertRelations(
                       rels: List[(ObjId, RelationTable, ObjId)],
                       commit: Commit
                     ): SQLFuture[Unit] = {
    val relations = rels.collectLists{case (l, rel, r) => rel -> (l, r)}
    val queries = for {
      groupedRelations <- relations
    } yield groupedRelations

    val queryStrings = queries.flatMap {
      case (rel, pair) =>
        pair.map {
          case (l, r) =>
            rel.insertRelation(l, r, commit)
        }
    }
    instance.writeBatch(queryStrings)
  }


  private def getLeftIds(
                  leftTable: ObjectTable,
                  t: TraversableOnce[(DBObject, RelationTable, DBObject)]
                ): SQLFuture[List[(ObjId, RelationTable, DBObject)]] =
    t.foldRight(SQLFutureI[List[(ObjId, RelationTable, DBObject)]](List())) {
      case ((lObject, relTable, rObject), cfList) =>
        for {
          lId <- leftTable.insertOrGetObject(lObject)
          list <-cfList
        } yield (lId, relTable, rObject) :: list
    }

  private def getRightIds(
                  rightTable: ObjectTable,
                  t: List[(ObjId, RelationTable, DBObject)]
                 ): SQLFuture[List[(ObjId, RelationTable, ObjId)]] =
    t.foldRight(SQLFutureI[List[(ObjId, RelationTable, ObjId)]](List())) {
      case ((lId, relTable, rObject), cfList) =>
        for {
          rId <- rightTable.insertOrGetObject(rObject)
          list <- cfList
        } yield (lId, relTable, rId) :: list
    }

  private def getExistingRelations(
                                    t: Set[RelationTable],
                                    view: View
                          ): SQLFuture[Set[(ObjId, RelationTable, ObjId)]] = {
      val sets = for {
        rel <- t
      } yield rel.getExistingRelations(view).map(_.map{case(l, r) => (l, rel, r)})

      ConstrainedFuture.sequence(sets).map(_.flatten)
    }
}


