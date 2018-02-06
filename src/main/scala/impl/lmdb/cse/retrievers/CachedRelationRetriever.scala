package impl.lmdb.cse.retrievers

import impl.lmdb.common.LMDBEither
import impl.lmdb.common.access.ObjId

import scala.collection.mutable
import core.utils._

/**
  * Created by Al on 06/02/2018.
  */
class CachedRelationRetriever(
                               lookup: Set[ObjId] => LMDBEither[Set[(ObjId, ObjId)]],
                               simpleLookup: ObjId => LMDBEither[Set[ObjId]]
                             ) extends RelationRetriever {

  private val memo = new mutable.HashMap[ObjId, Set[ObjId]]()
  private val inMemo = new mutable.HashSet[ObjId]()
  override def find(from: Set[ObjId]): LMDBEither[Set[(ObjId, ObjId)]] = {
    val alreadyFound = from intersect inMemo
    val needToBeFound = from diff inMemo
    val newResults = lookup(needToBeFound)
    val cachedResults = alreadyFound.flatMap(o => memo(o).map(o -> _))

    newResults.foreach{
      correctResults =>
        val resDict = correctResults.collectSets(identity)
        memo ++= resDict
        inMemo ++= resDict.keySet
    }
    for {
      r1 <- newResults
    } yield r1 ++ cachedResults
  }
  override def findRight(from: ObjId): LMDBEither[Set[ObjId]] =
    if (from in memo) LMDBEither(memo(from))
    else {
      for {
        res <- simpleLookup(from)
        _ = inMemo += from
        _ = memo += from -> res
      } yield res
    }
}
