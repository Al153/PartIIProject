package db.common

import utils._

/**
  * Created by Al on 20/10/2017.
  */

case class DBObject(fields: Vector[DBCell]) {
  override def toString: String = "|" + fields.map(_.truncate(9)).mkString("|") + "|"
}


