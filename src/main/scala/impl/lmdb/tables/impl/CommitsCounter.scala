package impl.lmdb.tables.impl

import impl.lmdb.LMDBInstance
import impl.lmdb.access.Key._
import impl.lmdb.access.{Commit, Key}
import impl.lmdb.tables.interfaces.MutableCounter
/**
  * Created by Al on 28/12/2017.
  */
class CommitsCounter(implicit val instance: LMDBInstance) extends MutableCounter[Commit] {
  override def path: Key = "db".key :: "nextCommit".key

  override protected def initialVales: Commit = Commit(0)
}
