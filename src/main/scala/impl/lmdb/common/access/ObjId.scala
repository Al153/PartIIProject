package impl.lmdb.common.access

import java.nio.ByteBuffer

import impl.lmdb.common.LMDBEither
import impl.lmdb.common.access.Storable.StorableLong

/**
  * Created by Al on 28/12/2017.
  *
  * ObjId provides an id for each object in LMDB database
  * This allows us to store the fields of an object separate to lookup and relation traversal tables
  */
case class ObjId(id: Long) extends AnyVal {
  /**
    * Get next available ObjId
    */
  def increment: ObjId = ObjId(id + 1)
}

object ObjId {

  /**
    * Object Ids should be able to be stored in the DB and also used as keys
    */
  implicit object ObjIdKeyable extends Keyable[ObjId] with ConstantLengthStorable[ObjId] {
    /**
      * Storeable methods hijack Storeable long
      */


    /**
      * Use a shorter value for keys
      */
    override def bytes(k: ObjId): Array[Byte] = BigInt(k.id).toByteArray

    /**
      * Expected length in buffer
      *
      * @return
      */
    override def length: Int = 8

    /**
      * Storeable objects need to be able to be converted to bytes to be stored
      */
    override def writeToExistingBuffer(a: ObjId, buf: ByteBuffer): Unit = StorableLong.writeToExistingBuffer(a.id, buf)

    /**
      * Storeable objects need to be extracted from a series of bytes
      *
      * @param buf - buffer to extract from
      * @return
      */
    override def fromBuffer(buf: ByteBuffer): LMDBEither[ObjId] = StorableLong.fromBuffer(buf).map(ObjId.apply)
  }
}
