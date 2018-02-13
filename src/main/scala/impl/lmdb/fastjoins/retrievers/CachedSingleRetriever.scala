package impl.lmdb.fastjoins.retrievers

import impl.lmdb.common.LMDBEither
import impl.lmdb.common.access.ObjId


/**
  * Created by Al on 06/02/2018.
  */
class CachedSingleRetriever(lookup: => LMDBEither[Set[ObjId]]) extends SingleRetriever {
  override lazy val find: LMDBEither[Set[ObjId]] = lookup
}
