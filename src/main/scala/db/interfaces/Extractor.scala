package db.interfaces

import db.common.{DBCell, DBObject}

import scalaz.\/

/**
  * Created by Al on 20/10/2017.
  * an extractor is something that gets values out of a database
  */
trait Extractor[A] {
  def fromRow(row: Vector[DBCell]): ExtractError \/ A
}

object  Extractor {
  implicit def pair[A, B](implicit a: Extractor[A], b: Extractor[B]) = new Extractor[(A, B)] {
    override def fromRow(row: Vector[DBCell]): \/[ExtractError, (A, B)] = ???
  }
}
