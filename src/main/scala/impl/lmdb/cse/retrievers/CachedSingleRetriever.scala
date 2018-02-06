package impl.lmdb.cse.retrievers

import core.backend.intermediate.unsafe._
import core.utils._
import impl.lmdb.common.LMDBEither
import impl.lmdb.common.access.{Commit, ObjId}
import impl.lmdb.common.interfaces.LMDBInstance


/**
  * Created by Al on 06/02/2018.
  */
class CachedSingleRetriever(lookup: => LMDBEither[Set[ObjId]]) extends SingleRetriever {
  override lazy val find: LMDBEither[Set[ObjId]] = lookup
}
