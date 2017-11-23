package impl.sql.tables

import core.containers.ConstrainedFuture
import core.error.E
import impl.sql.types.Commit

class CommitsRegistry {
  def getNewcommitId: ConstrainedFuture[E, Commit] = ???
}