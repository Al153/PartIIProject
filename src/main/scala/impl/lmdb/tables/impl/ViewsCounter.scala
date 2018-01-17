package impl.lmdb.tables.impl

import java.nio.ByteBuffer

import core.user.dsl.View
import impl.lmdb.access.Key._
import impl.lmdb.tables.interfaces.MutableCounter
import impl.lmdb.{LMDBInstance, _}
import org.lmdbjava.Dbi
import org.lmdbjava.DbiFlags._

/**
  * Created by Al on 28/12/2017.
  *
  * Simple MutableCounter for creating new views
  */

class ViewsCounter(implicit val instance: LMDBInstance) extends MutableCounter[View]("views".key) {
  override def name: String = "db:nextView"
  override val db: Dbi[ByteBuffer] = instance.env.openDbi(name, MDB_CREATE)
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
