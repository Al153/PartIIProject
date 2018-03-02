package remote

import core.user.containers.ConstrainedFuture
import core.user.dsl.E

package object util {
  implicit class CFResultOps(cf: ConstrainedFuture[E, Option[Unit]]) {
    def logRecovery: ConstrainedFuture[E, Option[Unit]] = {
      cf.recover {
        e =>
          util.logError(e)
          Some(())
      }
    }
  }

  def logError(e: E): Unit = {
    logger.error("Hit an error when setting up instances. Halting tests")
    logger.error(e.toString)
  }
}