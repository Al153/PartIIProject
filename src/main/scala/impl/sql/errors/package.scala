package impl.sql

import java.sql.SQLException

import core.user.dsl.HasRecovery



package object errors {

  implicit object SQLRecovery extends HasRecovery[SQLError] {
    /**
      * simple recovery function
      */
    def recover(e: Throwable): SQLError = {
      e match {
        case s: SQLException => CaughtSQLException(s)
        case t: Throwable => UnknownSQLException(t)
      }
    }
  }
}