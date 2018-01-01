package impl.lmdb.tables.impl

import core.view.View
import impl.lmdb.access.Key
import impl.lmdb.access.Key._
import impl.lmdb.tables.interfaces.LMDBTable
import impl.lmdb.{LMDBFuture, LMDBInstance}

/**
  * Created by Al on 28/12/2017.
  *
  * Holds the default view of the table
  */
class DefaultViewTable(implicit val instance: LMDBInstance) extends LMDBTable {
  override def path: Key =  "db".key :: "default".key

  def getDefault(): LMDBFuture[View] = ???
  def setDefault(v: View): LMDBFuture[Unit] = ???
}
