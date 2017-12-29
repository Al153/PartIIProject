package impl.lmdb.tables.impl

import core.view.View
import impl.lmdb.LMDBInstance
import impl.lmdb.access.Key
import impl.lmdb.access.Key._
import impl.lmdb.tables.interfaces.MutableCounter

/**
  * Created by Al on 28/12/2017.
  */

class ViewsCounter(implicit val instance: LMDBInstance) extends MutableCounter[View] {
  override val path: Key = "db".key :: "nextView".key
}
