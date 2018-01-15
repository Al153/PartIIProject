package impl.lmdb.tables.impl

import impl.lmdb.LMDBInstance
import impl.lmdb.access.Commit
import impl.lmdb.access.Key._
import impl.lmdb.tables.interfaces.MutableCounter
import org.fusesource.lmdbjni.Database

import scala.language.postfixOps
/**
  * Created by Al on 28/12/2017.
  *
  * A mutable counter for giving commits unique IDs
  */
class CommitsCounter(implicit val instance: LMDBInstance) extends MutableCounter[Commit]("Commits".key) {
  override val name: String = "db:nextCommit"

  override val db: Database = instance.env.openDatabase(name)

  /**
    * Initial commit is the zero commit
    */
  override protected def initialValue: Commit = Commit(0)

  /**
    * Simply increment the commit
    * @param a - old value
    * @return
    */
  override def next(a: Commit): Commit = a.increment
}
