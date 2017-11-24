package impl.sql.jdbc

import core.backend.common.DBObject
import core.containers.ConstrainedFuture
import core.error.E
import core.utils._
import core.view.View
import impl.sql._
import impl.sql.tables.{ObjectTable, RelationTable, ViewsTable}
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


  def insertObjects(
                     view: View,
                     commit: Commit,
                     t: TraversableOnce[(DBObject, ObjectTable, RelationTable, DBObject, ObjectTable)]
                   ): ConstrainedFuture[E, Unit] = {
    // insert a bunch of objects
    val precomputedViewName = PrecomputedView()
    for {
      _ <- presetup(view, precomputedViewName)
      withLeftIds <- getLeftIds(view, commit, t)
      withRightIds <- getRightIds(view, commit, withLeftIds)
      preExisting <- getExistingRelations(view, withRightIds.mapProj2.toSet)
      toAdd = withRightIds.filter {_ notIn preExisting}
      res <- insertRelations(toAdd, commit)
      _ <- cleanupViews(precomputedViewName)
    } yield res
  }

  private def insertRelations(
                       rels: List[(ObjId, RelationTable, ObjId)],
                       commit: Commit
                     ): ConstrainedFuture[E, Unit] = {
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

  // pre-emptively create a view to be used to do the writing
  private def presetup(v: View, precomputedViewName: PrecomputedView): ConstrainedFuture[E, Unit] = {
    val query = ViewsTable.usingView(v, precomputedViewName)
    instance.doWrite(query)
  }

  private def cleanupViews(name: PrecomputedView): ConstrainedFuture[E, Unit] =
    instance.viewsTable.removeTempViewOp(name)


  private def getLeftIds(
                  view: View,
                  commit: Commit,
                  t: TraversableOnce[(DBObject, ObjectTable, RelationTable, DBObject, ObjectTable)]
                ): ConstrainedFuture[E, List[(ObjId, RelationTable, DBObject, ObjectTable)]] =
    t.foldRight(ConstrainedFuture.immediatePoint[E, List[(ObjId, RelationTable, DBObject, ObjectTable)]](List())) {
      case ((lObject, lTable, relTable, rObject, rTable), cfList) =>
        for {
          lId <- lTable.insertOrGetObject(lObject, view, commit)
          list <-cfList
        } yield (lId, relTable, rObject, rTable) :: list
    }

  private def getRightIds(
                  view: View,
                  commit: Commit,
                  t: List[(ObjId, RelationTable, DBObject, ObjectTable)]
                 ): ConstrainedFuture[E, List[(ObjId, RelationTable, ObjId)]] =
    t.foldRight(ConstrainedFuture.immediatePoint[E, List[(ObjId, RelationTable, ObjId)]](List())) {
      case ((lId, relTable, rObject, rTable), cfList) =>
        for {
          rId <- rTable.insertOrGetObject(rObject, view, commit)
          list <-cfList
        } yield (lId, relTable, rId) :: list
    }

  private def getExistingRelations(
                            view: View, t: Set[RelationTable]
                          ): ConstrainedFuture[E, Set[(ObjId, RelationTable, ObjId)]] = {
      val sets = for {
        rel <- t
      } yield rel.getExistingRelations(view).map(_.map{case(l, r) => (l, rel, r)})
      ConstrainedFuture.sequence(sets).map(_.flatten)
    }



}


