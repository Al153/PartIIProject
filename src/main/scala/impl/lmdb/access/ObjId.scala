package impl.lmdb.access

import core.view.View

/**
  * Created by Al on 28/12/2017.
  */
case class ObjId(id: Long) extends AnyVal {}

object ObjId {
  implicit object ObjIdKeyable extends Keyable[ObjId] with Storeable[ObjId] {
    override def bytes(k: ObjId): Array[Byte] = BigInt(k.id).toByteArray
    override def toBytes(o: ObjId): Array[Byte] = BigInt(o.id).toByteArray
  }
}
