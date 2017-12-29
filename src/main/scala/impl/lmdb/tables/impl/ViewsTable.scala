package impl.lmdb.tables.impl

import core.view.View
import impl.lmdb.LMDBInstance
import impl.lmdb.access.Key._
import impl.lmdb.access.{Commit, Key}
import impl.lmdb.containers.{Extractor, Inserter}
import impl.lmdb.tables.interfaces.LMDBTable

/**
  * Created by Al on 28/12/2017.
  */
class ViewsTable(implicit val instance: LMDBInstance) extends LMDBTable {
  override def path: Key = "db".key :: "views".key

  def lookupCommit(v: View): Extractor[Commit] = ???
  def newChildView(old: View, newView: View, commit: Commit): Inserter[View] = ???
}
