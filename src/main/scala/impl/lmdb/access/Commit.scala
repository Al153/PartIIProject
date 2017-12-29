package impl.lmdb.access

/**
  * Created by Al on 28/12/2017.
  */
case class Commit(id: Long) extends AnyVal

object Commit {
  implicit object StoreableCommit extends Storeable[Commit] {
    override def toBytes(c: Commit): Array[Byte] = BigInt(c.id).toByteArray
  }
}