package impl.lmdb.tables.impl

import core.user.dsl.View
import impl.lmdb.LMDBInstance
import impl.lmdb.access.Key
import impl.lmdb.access.Key._
import impl.lmdb.tables.interfaces.MutableCounter
import impl.lmdb._
/**
  * Created by Al on 28/12/2017.
  */

class ViewsCounter(implicit val instance: LMDBInstance) extends MutableCounter[View] {
  override val path: Key = "db" >> "nextView"
  override protected def initialValue: View = next(initialView)
  override protected def next(a: View): View = View(a.id + 1)

  initialise()




}
