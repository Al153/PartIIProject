package db.common

import utils._

/**
  * Created by Al on 20/10/2017.
  */

case class DBObject(fields: Vector[DBCell]) {
  override def toString: String = "|" + fields.map(_.truncate(9)).mkString("|") + "|"
}


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

