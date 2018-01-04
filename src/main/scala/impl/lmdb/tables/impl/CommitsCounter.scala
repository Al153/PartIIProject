package impl.lmdb.tables.impl

import impl.lmdb.LMDBInstance
import impl.lmdb.access.Key._
import impl.lmdb.access.{Commit, Key}
import impl.lmdb.tables.interfaces.MutableCounter

import scala.language.postfixOps
/**
  * Created by Al on 28/12/2017.
  */
class CommitsCounter(implicit val instance: LMDBInstance) extends MutableCounter[Commit] {
  override val path: Key = "db" >> "nextCommit"

  override protected def initialValue: Commit = Commit(0)

  override def next(a: Commit): Commit = a.increment

  initialise()
}
