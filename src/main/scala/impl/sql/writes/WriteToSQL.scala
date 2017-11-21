package impl.sql.writes

import core.backend.common.DBObject
import core.containers.ConstrainedFuture
import core.error.E
import core.relations.CompletedRelation
import impl.sql.{ObjectTableName, RelationTableName, SQLTableName}
import impl.sql.view.Commit
import core.utils._

import scala.reflect.internal.util.Statistics.View

/**
  * Created by Al on 21/11/2017.
  */
object WriteToSQL {
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
    val withLeftIds: ConstrainedFuture[E, TraversableOnce[(ObjId, RelationTableName, DBObject, ObjectTableName)]] = ???
    val withRightIds: ConstrainedFuture[E, TraversableOnce[(ObjId, RelationTableName, ObjId)]] = ???
    // find a set of the relevant relations that already exist
    val existingRelations: ConstrainedFuture[E, Set[(ObjId, ObjId)]] = ???

    val toAdd = for {
      all <- withRightIds
      existing <- existingRelations

    } yield all.filter {case (l, rel, r) => (l, r) notIn existing}

    toAdd.flatMap(ta => insertRelations(ta, commit))
  }

  def insertRelations(rels: TraversableOnce[(ObjId, RelationTableName, ObjId)], commit: Commit): ConstrainedFuture[E, Unit] = {
    val fullQuery = rels map {case (left, rel, right) => createInsertRelation(left, rel, right, commit)} mkString "\n"
    ??? // todo: Execute
  }

  def createInsertRelation(leftId: ObjId, table: RelationTableName, rightId: ObjId, commit: Commit): String =
    s"INSERT INTO $table (???) VALUES ('${leftId.id}', '${commit.id}', ${rightId.id});"
}


case class ObjId(id: Long) extends AnyVal