package impl.lmdb.access

import impl.lmdb.LMDBEither
import impl.lmdb.access.Storeable.StoreableLong

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
  implicit object ObjIdKeyable extends Keyable[ObjId] with Storeable[ObjId] {
    /**
      * Storeable methods hijack Storeable long
      */
    override def toBytes(o: ObjId): Vector[Byte] = StoreableLong.toBytes(o.id)

    override def fromBytes(bytes: Vector[Byte]): LMDBEither[ObjId] = StoreableLong.fromBytes(bytes).map(ObjId.apply)

    /**
      * Use a shorter value for keys
      */
    override def bytes(k: ObjId): Array[Byte] = BigInt(k.id).toByteArray
  }
}
