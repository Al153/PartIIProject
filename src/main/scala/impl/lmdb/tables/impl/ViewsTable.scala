package impl.lmdb.tables.impl

import core.user.dsl.View
import impl.lmdb.access.Key._
import impl.lmdb.access.{Commit, Key}
import impl.lmdb.tables.interfaces.LMDBTable
import impl.lmdb.{LMDBEither, LMDBInstance}
import impl.lmdb._
import impl.lmdb.access.Storeable._

/**
  * Created by Al on 28/12/2017.
  *
  * Relates views to commits
  */
// todo: move away from the Set[Commit], since we only ever append to the collection of commits or traverse it.
//       so a list might be quicker
// todO: These values are immutable so can be easily cached
class ViewsTable(implicit val instance: LMDBInstance) extends LMDBTable {
  override def path: Key = "db" >> "views"

  /**
    * Initialisation consists of setting the initial view to contain the empty set of commits
    */
  override def initialise(): LMDBEither[Unit] = newChildView(initialView, Set())

  /**
    * Lokup commits associated with a view
    * @param v
    * @return
    */
  def lookupCommits(v: View): LMDBEither[Set[Commit]] = get(path >> v)

  /**
    * Insert a new child view  with its commits
    */
  def newChildView(newView: View, commits: Set[Commit]): LMDBEither[Unit] = put(path >> newView, commits)
}
