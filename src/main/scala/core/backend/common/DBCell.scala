package core.backend.common

/**
  * Created by Al on 05/11/2017.
  *
  *  A simple type hierarchy to allow easy conversion of each of the primitive types into storeable values
  */

sealed trait DBCell
case class DBInt(i: Int) extends DBCell {
  override def toString: String = i.toString
}
case class DBString(s: String) extends DBCell {
  override def toString: String = s
}
case class DBBool(b: Boolean) extends DBCell {
  override def toString: String = b.toString
}
case class DBDouble(d: Double) extends DBCell {
  override def toString: String = d.toString
}