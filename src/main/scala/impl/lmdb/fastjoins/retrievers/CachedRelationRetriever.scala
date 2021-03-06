package impl.lmdb.fastjoins.retrievers

import core.utils._
import impl.lmdb.common.LMDBEither
import impl.lmdb.common.access.ObjId


import scala.collection.mutable

/**
  * Created by Al on 06/02/2018.
  */
class CachedRelationRetriever(
                               lookup: Set[ObjId] =>  LMDBEither[Map[ObjId, Set[ObjId]]],
                               simpleLookup: ObjId => LMDBEither[Set[ObjId]]
                             ) extends RelationRetriever with Logged {
  private var memoHit: Int = 0
  private var memoMiss: Int = 0

  private val memo = new mutable.HashMap[ObjId, Set[ObjId]]()
  override def find(from: Set[ObjId]): LMDBEither[Map[ObjId, Set[ObjId]]] = {
    val alreadyFound = from intersect memo.keySet
    val needToBeFound = from diff memo.keySet
    val cachedResults = memo.filterKeys(alreadyFound)

    logger.info(s"Arg size: ${from.size}; memo'd ${alreadyFound.size}; eval-ing: ${needToBeFound.size}")
    if (needToBeFound.isEmpty) {
      logger.info("Doing lookup")
      LMDBEither(cachedResults.toMap)
    }
    else {
      logger.info("Doing lookup")
      val newResults = lookup(needToBeFound)
      logger.info("Done lookup")
      newResults.foreach{
        correctResults =>
          memo ++= correctResults
      }
      for {
        r1 <- newResults
      } yield r1 ++ cachedResults
    }
  }

  override def findFrom(from: ObjId): LMDBEither[Set[ObjId]] =
      if (from in memo){
        memoHit += 1
        if (memoHit % 1000 == 0) {
          logger.info("Memo hits: " + memoHit)
        }
        LMDBEither(memo(from))
      }
      else {
        memoMiss += 1
        if (memoMiss % 1000 == 0) {
          logger.info("Memo misses: " + memoMiss)
        }
        for {
          res <- simpleLookup(from)
          _ = memo += from -> res
        } yield res
      }

}
