package core.backend.common

import core.utils._

/**
  * Created by Al on 20/10/2017.
  *
  *  Any object with a SchemaObject can be converted into a vector of DBCell Fields
  */

case class DBObject(fields: Vector[DBCell]) {
  override def toString: String = "|" + fields.map(_.truncate(9)).mkString("|") + "|"
}


