package impl.sql.writes

import core.backend.common.DBObject
import core.containers.ConstrainedFuture
import core.error.E
import core.utils._
import core.view.View
import impl.sql.errors.MissingSQLRelation
import impl.sql.tables.{RelationTable, ViewsTable}
import impl.sql.types.{Commit, ObjId}
import impl.sql.{ObjectTableName, RelationTableName, SQLInstance}


/**
  * Created by Al on 21/11/2017.
  */
class WriteToSQL(instance: SQLInstance) {
  // todo:

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

  // search for an appropriate object in the view, if there isn't, insert one to the new commit. return the ObjId
  private def insertOrGetObject(table: ObjectTableName, dBObject: DBObject, view: View, newCommit: Commit): ConstrainedFuture[E, ObjId] = ???

  def insertObjects(view: View, commit: Commit, t: TraversableOnce[(DBObject, ObjectTableName, RelationTableName, DBObject, ObjectTableName)]): ConstrainedFuture[E, Unit] = {
    // insert a bunch of objects
    for {
      _ <- presetup(view)
      withLeftIds <- getLeftIds(view, commit, t)
      withRightIds <- getRightIds(view, commit, withLeftIds)
      preExisting <- getExistingRelations(view, withRightIds.mapProj2.toSet)
      toAdd = withRightIds.filter {_ notIn preExisting}
      res <- insertRelations(toAdd, commit)
    } yield res
  }

  def insertRelations(
                       rels: List[(ObjId, RelationTableName, ObjId)],
                       commit: Commit
                     ): ConstrainedFuture[E, Unit] = {
    val relations = rels.collectLists{case (l, rel, r) => rel -> (l, r)}
    val queries = for {
      groupedRelations <- EitherOps.sequence(relations.map {
        case (rel, list) => instance.relationTables.getOrError(rel, MissingSQLRelation(rel)).withSnd(list)
      })
    } yield groupedRelations.flatMap { case (rel, pair) => pair.map { case (l, r) => rel.insertRelation(l, r, commit) } }

    ??? // todo: Execute
  }

  // pre-emptively create a view to be used to do the writing
  private def presetup(v: View): ConstrainedFuture[E, Unit] = {
    val query = ViewsTable.usingView(v)("")
    ???
  }

  def getLeftIds(
                  view: View,
                  commit: Commit,
                  t: TraversableOnce[(DBObject, ObjectTableName, RelationTableName, DBObject, ObjectTableName)]
                ): ConstrainedFuture[E, List[(ObjId, RelationTableName, DBObject, ObjectTableName)]] = ???

  def getRightIds(
                  view: View,
                  commit: Commit,
                  t: List[(ObjId, RelationTableName, DBObject, ObjectTableName)]
                 ): ConstrainedFuture[E, List[(ObjId, RelationTableName, ObjId)]] = ???

  def getExistingRelations(view: View, t: Set[RelationTableName]): ConstrainedFuture[E, Set[(ObjId, RelationTableName, ObjId)]] = ???


}


