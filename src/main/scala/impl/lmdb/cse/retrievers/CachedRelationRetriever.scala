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
  override def find(from: Set[ObjId]): LMDBEither[Set[(ObjId, ObjId)]] = {
    val alreadyFound = from intersect memo.keySet
    val needToBeFound = from diff memo.keySet
    val newResults = lookup(needToBeFound)
    val cachedResults = alreadyFound.flatMap(o => memo(o).map(o -> _))

    newResults.foreach{
      correctResults =>
        val resDict = correctResults.collectSets(identity)
        memo ++= resDict
    }
    for {
      r1 <- newResults
      _ = println("New results: " + r1)
      _ = println("Old results: " + cachedResults)
    } yield r1 ++ cachedResults
  }

  override def findRight(from: ObjId): LMDBEither[Set[ObjId]] =
    if (from in memo) LMDBEither(memo(from))
    else {
      for {
        res <- simpleLookup(from)
        _ = memo += from -> res
      } yield res
    }
}
