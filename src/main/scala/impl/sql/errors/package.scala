package impl.sql

import java.sql.SQLException

package object errors {
  def recoverSQLException(e: Throwable): SQLError = {
    e match {
      case s: SQLException => CaughtSQLException(s)
      case t: Throwable => UnknownSQLException(t)
    }
  }
}