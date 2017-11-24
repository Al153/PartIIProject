package impl.sql.errors

import java.sql.SQLException

import core.error.E

case class CaughtSQLException(e: SQLException) extends SQLError {
  override def toString: String = "CAUGHT SQL EXCEPTION: " + e.getStackTrace.mkString("\n")
}