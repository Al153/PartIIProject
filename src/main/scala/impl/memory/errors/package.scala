package impl.memory

import core.user.dsl.HasRecovery

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

  implicit object MemoryRecovery extends HasRecovery[MemoryError] {
    override def recover(t: Throwable): MemoryError = CaughtMemoryException(t)
  }
}
