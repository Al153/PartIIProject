package impl.sql.tables

import core.containers.ConstrainedFuture
import core.error.E
import impl.sql.types.Commit

object CommitsRegistry {
  def getNewcommitId: ConstrainedFuture[E, Commit] = ???
}