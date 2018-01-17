package impl.lmdb.access
import java.nio.ByteBuffer

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
    /**
      * Expected length in buffer
      *
      * @return
      */
    override def bufferLength(a: Commit): Int = 8 // length of a long

    /**
      * Storeable objects need to be able to be converted to bytes to be stored
      */
    override def writeToBuffer(a: Commit, buf: ByteBuffer): Unit = buf.putLong(a.id)

    /**
      * Storeable objects need to be extracted from a series of bytes
      *
      * @param buf - buffer to extract from
      * @return
      */
    override def fromBuffer(buf: ByteBuffer): LMDBEither[Commit] = StoreableLong.fromBuffer(buf).map(Commit.apply)
}
}