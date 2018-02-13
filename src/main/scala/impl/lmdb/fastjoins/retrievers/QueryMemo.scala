package impl.lmdb.fastjoins.retrievers

import core.backend.intermediate.unsafe._
import core.utils._

/**
  * Created by Al on 06/02/2018.
  */
class QueryMemo(val pairs: Map[UnsafeFindPair, RelationRetriever], val singles: Map[UnsafeFindSingle, SingleRetriever]) extends Logged {
  def get(q: UnsafeFindPair, fallback: => (QueryMemo, RelationRetriever)): (QueryMemo, RelationRetriever) =
    {
      if (q in pairs) {
        logger.trace("Eliminated common subexpressions")
        (this, pairs(q))
      }
      else {
        logger.trace("Creating new subexpression")
        val (resMemo, r) = fallback
        val newMemo = new QueryMemo(this.pairs ++ resMemo.pairs + (q -> r), this.singles ++ resMemo.singles)
        (newMemo, r)
      }
    }

  def get(q: UnsafeFindSingle, fallback: => (QueryMemo, SingleRetriever)): (QueryMemo, SingleRetriever) = {
    if (q in singles) {
      logger.trace("Eliminated subexpression")
      (this, singles(q))
    }
    else {
      logger.trace("Creating new subexpression")
      val (resMemo, r) = fallback
      val newMemo = new QueryMemo(this.pairs ++ resMemo.pairs, this.singles ++ resMemo.singles + (q -> r))
      (newMemo, r)
    }
  }
}
