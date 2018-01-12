package impl.sql

import java.sql.SQLException



package object errors {
  /**
    * simple recovery function
    */
  def recoverSQLException(e: Throwable): SQLError = {
    e match {
      case s: SQLException => CaughtSQLException(s)
      case t: Throwable => UnknownSQLException(t)
    }
  }
}