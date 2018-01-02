package impl.lmdb.access
import impl.lmdb.LMDBEither
import impl.lmdb.access.Storeable.StoreableLong

/**
  * Created by Al on 28/12/2017.
  */
case class Commit(id: Long) extends AnyVal {
  def ++ : Commit = new Commit(id + 1)
}

object Commit {
  implicit object StoreableCommit extends Storeable[Commit]  {
    override def toBytes(c: Commit): Vector[Byte] = StoreableLong.toBytes(c.id)
    override def fromBytes(bytes: Vector[Byte]): LMDBEither[Commit] = StoreableLong.fromBytes(bytes).map(Commit.apply)
  }
}