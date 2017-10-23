package db.interfaces

import db.common.{DBObject, ExtractError}
import db.common
import scalaz.\/

/**
  * Created by Al on 20/10/2017.
  * an extractor is something that gets values out of a database
  */
trait Extractor[A] {
  def fromRow(row: DBObject): ExtractError \/ A
}

object Extractor {
  implicit def pair[A, B](implicit ea: Extractor[A], eb: Extractor[B]) = new Extractor[(A, B)] {
    override def fromRow(row: DBObject): \/[ExtractError, (A, B)] = row match {
      case common.Pair(left, right) => for {
        a <- ea.fromRow(left)
        b <- eb.fromRow(right)
      } yield (a, b)
    }
  }
}
