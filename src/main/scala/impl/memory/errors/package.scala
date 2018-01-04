package impl.memory

/**
  * Created by Al on 04/01/2018.
  */
package object errors {
  def recoverMemoryException(e: Throwable): MemoryError = CaughtMemoryException(e)
}
