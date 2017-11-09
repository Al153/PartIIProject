package impl.sql.adt

import core.intermediate.unsafe.UnsafeFindable
import impl.sql.SQLTableName
import impl.sql.adt.SQLPair

// queries expose a single id to join on
sealed trait SQLSingle
case class All(table: SQLTableName) extends SQLSingle
case class Find(f: UnsafeFindable) extends SQLSingle
case class NarrowS(left: SQLSingle, f: UnsafeFindable) extends SQLSingle
case class From(start: SQLSingle, rel: SQLPair) extends SQLSingle

object SQLSingle {
  def getQueryString(s: SQLSingle): String = ???
}