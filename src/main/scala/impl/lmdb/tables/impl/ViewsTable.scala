package impl.lmdb.tables.impl

import core.user.dsl.View
import impl.lmdb.access.Commit
import impl.lmdb.access.Key._
import impl.lmdb.access.Storeable._
import impl.lmdb.tables.interfaces.LMDBTable
import impl.lmdb.{LMDBEither, LMDBInstance, _}
import org.fusesource.lmdbjni.Database

/**
  * Created by Al on 28/12/2017.
  *
  * Relates views to commits
  */
// todo: move away from the Set[Commit], since we only ever append to the collection of commits or traverse it.
//       so a list might be quicker
// todO: These values are immutable so can be easily cached
class ViewsTable(implicit val instance: LMDBInstance) extends LMDBTable {
  override def name: String = "db:views"
  override val db: Database = instance.env.openDatabase(name)

  /**
    * Initialisation consists of setting the initial view to contain the empty set of commits
    */
  override def initialise(): LMDBEither[Unit] = newChildView(initialView, Set())

  /**
    * Lookup commits associated with a view
    * @param v
    * @return
    */
  def lookupCommits(v: View): LMDBEither[Set[Commit]] = get(db, v.key)

  /**
    * Insert a new child view  with its commits
    */
  def newChildView(newView: View, commits: Set[Commit]): LMDBEither[Unit] = put(newView.key, commits, db)
}
