package impl.lmdb.tables.impl

import core.user.dsl.View
import impl.lmdb.LMDBInstance
import impl.lmdb.access.Key
import impl.lmdb.access.Key._
import impl.lmdb.tables.interfaces.MutableCounter
import impl.lmdb._
/**
  * Created by Al on 28/12/2017.
  *
  * Simple MutableCounter for creating new views
  */

class ViewsCounter(implicit val instance: LMDBInstance) extends MutableCounter[View] {
  override val path: Key = "db" >> "nextView"
  /**
    * Initial value is Succ(initialView) to avoid collisions (initialView is already in the table)
    */
  override protected def initialValue: View = next(initialView)

  /**
    * Simply increment old value
    * @param a - old value
    * @return
    */
  override protected def next(a: View): View = View(a.id + 1)
}
