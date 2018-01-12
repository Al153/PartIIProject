package impl.sql.errors

import java.sql.SQLException

/**
  * Contains an exception caught from the JDBC interface
  */
case class CaughtSQLException(e: SQLException) extends SQLError {
  override def toString: String = "CAUGHT SQL EXCEPTION: " + e.getMessage + "\n" + e.getStackTrace.mkString("\n")
}