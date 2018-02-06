package impl.lmdb.cse.retrievers
import core.backend.intermediate.unsafe._
import impl.lmdb.common.access.{Commit, ObjId}
import core.utils._
import impl.lmdb.common._
import impl.lmdb.common.interfaces.LMDBInstance

import scalaz._
import Scalaz._
/**
  * Created by Al on 06/02/2018.
  */
trait SingleRetriever {
  def find: LMDBEither[Set[ObjId]]
  def into(that: RelationRetriever): SingleRetriever = new CachedSingleRetriever(
    find.flatMapS(that.findRight)
  )
  def and(that: SingleRetriever): SingleRetriever = new CachedSingleRetriever(
    for {
      as <- find
      bs <- that.find
    } yield as intersect bs
  )
  def or(that: SingleRetriever): SingleRetriever = new CachedSingleRetriever(
    for {
      as <- find
      bs <- that.find
    } yield as union bs
  )
}
