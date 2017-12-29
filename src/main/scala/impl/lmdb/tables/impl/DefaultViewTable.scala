package impl.lmdb.tables.impl

import core.view.View
import impl.lmdb.LMDBInstance
import impl.lmdb.access.Key
import impl.lmdb.access.Key._
import impl.lmdb.containers.SingleExtractor
import impl.lmdb.tables.interfaces.LMDBTable

/**
  * Created by Al on 28/12/2017.
  *
  * Holds the default view of the table
  */
class DefaultViewTable(implicit val instance: LMDBInstance) extends LMDBTable {
  override def path: Key =  "db".key :: "default".key

  def getDefault(): SingleExtractor[View] = ???
  def setDefault(v: View): SingleExtractor[Unit] = ???
}
