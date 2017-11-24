package impl.sql.adt

import core.error.E
import core.schema.SchemaObject

import scalaz.\/

sealed trait SQLResult {
  def extractSingle[A](implicit ex: SchemaObject[A]): E \/Vector[A]
  def extractPair[A, B](implicit sa: SchemaObject[A], sb: SchemaObject[B]): E \/Vector[(A, B)]
}