package impl.lmdb

/**
  * Created by Al on 28/12/2017.
  */
package object errors {

  /**
    * Catch exceptions in an LMDBFuture
    * @param e
    * @return
    */
  def recoverLMDBException(e: Throwable): LMDBError = {
    CaughtLMDBException(e)
  }
}
