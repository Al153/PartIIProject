package impl.lmdb.access

import impl.lmdb.LMDBEither
import impl.lmdb.access.Storeable.StoreableLong

/**
  * Created by Al on 28/12/2017.
  */
case class ObjId(id: Long) extends AnyVal {
  def increment: ObjId = ObjId(id + 1)
}

object ObjId {
  implicit object ObjIdKeyable extends Keyable[ObjId] with Storeable[ObjId] {
    override def bytes(k: ObjId): Array[Byte] = BigInt(k.id).toByteArray
    override def toBytes(o: ObjId): Vector[Byte] = StoreableLong.toBytes(o.id)

    override def fromBytes(bytes: Vector[Byte]): LMDBEither[ObjId] = StoreableLong.fromBytes(bytes).map(ObjId.apply)
  }
}
