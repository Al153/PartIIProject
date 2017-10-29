package db.common

/**
  * Created by Al on 20/10/2017.
  */

case class DBObject(fields: Vector[DBCell])


sealed trait DBCell
case class DBInt(i: Int) extends DBCell
case class DBString(s: String) extends DBCell
case class DBBool(b: Boolean) extends DBCell
case class DBDouble(d: Double) extends DBCell

