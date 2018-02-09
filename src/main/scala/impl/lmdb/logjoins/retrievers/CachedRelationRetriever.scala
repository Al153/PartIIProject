package impl.lmdb.logjoins.retrievers

import core.utils._
import impl.lmdb.common.LMDBEither
import impl.lmdb.common.access.ObjId
import impl.lmdb.logjoins._

import scala.collection.mutable

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

    logger.info(s"Arg size: ${from.size}; memo'd ${alreadyFound.size}; eval-ing: ${needToBeFound.size}")

    newResults.foreach{
      correctResults =>
        val resDict = correctResults.collectSets(identity)
        memo ++= resDict
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
        _ = memo += from -> res
      } yield res
    }
}
