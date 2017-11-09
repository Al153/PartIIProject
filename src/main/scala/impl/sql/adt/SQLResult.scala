package impl.sql.adt

import core.backend.interfaces.Extractor
import core.error.E

import scalaz.\/

sealed trait SQLResult {
  def extractSingle[A](implicit ex: Extractor[A]): E \/Vector[A]
  def extractPair[A, B](implicit ea: Extractor[A], eb: Extractor[B]): E \/Vector[(A, B)]
}