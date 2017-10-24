package db.common

/**
  * Created by Al on 20/10/2017.
  */
sealed trait DBObject
case class DBRow(fields: Vector[DBCell]) extends DBObject


sealed trait DBCell
case class DBInt(i: Int) extends DBCell
case class DBString(s: String) extends DBCell
case class DBBool(b: Boolean) extends DBCell
case class DBDouble(d: Double) extends DBCell

