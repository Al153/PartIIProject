package impl.lmdb.tables.impl

import impl.lmdb.LMDBInstance
import impl.lmdb.access.Key._
import impl.lmdb.access.{Key, ObjId}
import impl.lmdb.tables.interfaces.MutableCounter

/**
  * Created by Al on 29/12/2017.
  */
class ObjectsCounter(implicit val instance: LMDBInstance) extends MutableCounter[ObjId] {
  override def path: Key = "db".key :: "nextCommit".key

  override val initialVales = ObjId(0)
}
