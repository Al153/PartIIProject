package impl.memory

/**
  * Created by Al on 04/01/2018.
  *
  * recovery method holder
  */
package object errors {
  /**
    * Recovers exceptions
    * @return
    */
  def recoverMemoryException(e: Throwable): MemoryError = CaughtMemoryException(e)
}
