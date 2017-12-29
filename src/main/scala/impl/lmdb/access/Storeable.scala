package impl.lmdb.access

import core.view.View

/**
  * Created by Al on 29/12/2017.
  */
trait Storeable[A] {
  def toBytes(a: A): Array[Byte]
}

object Storeable {
  implicit object StoreableView extends Storeable[View] {
    override def toBytes(v: View): Array[Byte] = BigInt(v.id).toByteArray
  }
}