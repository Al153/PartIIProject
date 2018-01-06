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
  */
class ViewsTable(implicit val instance: LMDBInstance) extends LMDBTable {
  override def path: Key = "db" >> "views"

  newChildView(initialView, Set())

  def lookupCommits(v: View): LMDBEither[Set[Commit]] = get(path >> v)

  def newChildView(newView: View, commits: Set[Commit]): LMDBEither[Unit] = put(path >> newView, commits)
}
