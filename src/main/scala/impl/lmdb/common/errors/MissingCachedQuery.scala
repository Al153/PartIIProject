package impl.lmdb.common.errors

import core.backend.intermediate.unsafe.UnsafeFindSingle

/**
  * Occurs when a cached UnsafeFindSingle is missing
  * @param q
  */
case class MissingCachedQuery(q: UnsafeFindSingle) extends LMDBError