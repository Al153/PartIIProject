package impl.sql.errors

import java.sql.SQLException

import core.user.dsl.E

case class CaughtSQLException(e: SQLException) extends SQLError {
  override def toString: String = "CAUGHT SQL EXCEPTION: " + e.getMessage + "\n" + e.getStackTrace.mkString("\n")
}