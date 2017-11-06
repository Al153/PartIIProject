package core.backend.interfaces

import core.error.E
import core.backend.common.DBObject

import scalaz.\/

/**
  * Created by Al on 20/10/2017.
  * an extractor is something that gets values out of a database
  */
trait Extractor[A] {
  def fromRow(row: DBObject): E \/ A
}
