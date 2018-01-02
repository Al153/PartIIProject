package impl.lmdb.tables.impl

import impl.lmdb.LMDBInstance
import impl.lmdb.access.Key._
import impl.lmdb.access.{Key, ObjId}
import impl.lmdb.tables.interfaces.MutableCounter

import scala.language.postfixOps

/**
  * Created by Al on 29/12/2017.
  */
class ObjectsCounter(implicit val instance: LMDBInstance) extends MutableCounter[ObjId] {
  override def path: Key = "db".key :: "nextCommit".key

  override val initialValue = ObjId(0)

  override def next(a: ObjId): ObjId = a++
}
