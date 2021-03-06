package impl.lmdb.fastjoins.retrievers

import core.utils._
import impl.lmdb.common._
import impl.lmdb.common.access.ObjId
/**
  * Created by Al on 06/02/2018.
  */
trait SingleRetriever {
  def find: LMDBEither[Set[ObjId]]
}

object SingleRetriever {
  implicit class SingleRetrieverOps(outer: SingleRetriever) {
    def into(that: RelationRetriever): SingleRetriever = new CachedSingleRetriever(
      outer.find.flatMapS(that.findFrom)
    )
    def and(that: SingleRetriever): SingleRetriever = new CachedSingleRetriever(
      for {
        as <- outer.find
        bs <- that.find
      } yield as intersect bs
    )
    def or(that: SingleRetriever): SingleRetriever = new CachedSingleRetriever(
      for {
        as <- outer.find
        bs <- that.find
      } yield as union bs
    )
  }
}
