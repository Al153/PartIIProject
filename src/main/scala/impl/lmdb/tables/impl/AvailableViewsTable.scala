package impl.lmdb.tables.impl

import core.view.View
import impl.lmdb.LMDBInstance
import impl.lmdb.access.Key._
import impl.lmdb.access._
import impl.lmdb.containers.Extractor
import impl.lmdb.tables.interfaces.LMDBTable


/**
  * Created by Al on 28/12/2017.
  */
class AvailableViewsTable(implicit val instance: LMDBInstance) extends LMDBTable {
  override def path: Key = "db".key :: "views".key

  def getAvailableViews(): Extractor[View] = ???
}
