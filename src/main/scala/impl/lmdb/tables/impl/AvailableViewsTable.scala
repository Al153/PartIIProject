package impl.lmdb.tables.impl

import core.view.View
import impl.lmdb.access.Key._
import impl.lmdb.access._
import impl.lmdb.tables.interfaces.LMDBTable
import impl.lmdb.{LMDBEither, LMDBInstance}


/**
  * Created by Al on 28/12/2017.
  */
class AvailableViewsTable(implicit val instance: LMDBInstance) extends LMDBTable {
  override def path: Key = "db".key :: "views".key

  def getAvailableViews(): LMDBEither[Set[View]] = ???
  def validateView(v: View): LMDBEither[Unit] = ???
  def insertNewView(v: View): LMDBEither[Unit] = ???
}
