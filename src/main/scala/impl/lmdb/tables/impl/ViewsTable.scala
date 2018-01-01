package impl.lmdb.tables.impl

import core.view.View
import impl.lmdb.access.Key._
import impl.lmdb.access.{Commit, Key}
import impl.lmdb.tables.interfaces.LMDBTable
import impl.lmdb.{LMDBEither, LMDBInstance}

/**
  * Created by Al on 28/12/2017.
  */
class ViewsTable(implicit val instance: LMDBInstance) extends LMDBTable {
  override def path: Key = "db".key :: "views".key

  def lookupCommits(v: View): LMDBEither[Set[Commit]] = ???
  def newChildView(newView: View, commits: Set[Commit]): LMDBEither[Unit] = ???
}
