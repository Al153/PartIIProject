package impl.sql.errors

import core.error.E

case class UnknownSQLException(e: Throwable) extends SQLError {
  override def toString: String = "CAUGHT SQL EXCEPTION: " + e.getStackTrace.mkString("\n")
}