package impl.lmdb.common

import core.user.dsl.HasRecovery

/**
  * Created by Al on 28/12/2017.
  */
package object errors {


  implicit object LMDBRecover extends HasRecovery[LMDBError]{
    /**
      * Catch exceptions in an LMDBFuture
      * @param t
      * @return
      */
    override def recover(t: Throwable): LMDBError = CaughtLMDBException(t)
  }
}
