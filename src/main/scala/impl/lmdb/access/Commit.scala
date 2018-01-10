package impl.lmdb.access
import impl.lmdb.LMDBEither
import impl.lmdb.access.Storeable.StoreableLong

/**
  * Created by Al on 28/12/2017.
  *
  * Commit implementation: A view maps to a number of commits, representing groups of additions to the database
  */
case class Commit(id: Long) extends AnyVal {
  /**
    * get next commit (for updating [[impl.lmdb.tables.impl.CommitsCounter]])
    */
  def increment : Commit = new Commit(id + 1)
}

object Commit {

  /**
    * Storeable instance for Commits
    */
  implicit object StoreableCommit extends Storeable[Commit]  {
    /**
      * Hijack [[impl.lmdb.access.Storeable.StoreableLong]]'s methods
      */
    override def toBytes(c: Commit): Vector[Byte] = StoreableLong.toBytes(c.id)
    override def fromBytes(bytes: Vector[Byte]): LMDBEither[Commit] = StoreableLong.fromBytes(bytes).map(Commit.apply)
  }
}