package impl.lmdb.cse.retrievers
import impl.lmdb.common._
import impl.lmdb.common.access.ObjId
/**
  * Created by Al on 06/02/2018.
  */
class UncachedSingleRetriever(lookup: => LMDBEither[Set[ObjId]]) extends SingleRetriever {
  override def find: LMDBEither[Set[ObjId]] = lookup
}
