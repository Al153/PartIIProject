package impl.lmdb.logjoins.retrievers

import core.backend.intermediate.unsafe._
import core.utils._

/**
  * Created by Al on 06/02/2018.
  */
class QueryMemo(val pairs: Map[UnsafeFindPair, RelationRetriever], val singles: Map[UnsafeFindSingle, SingleRetriever]) {
  def get(q: UnsafeFindPair, fallback: => (QueryMemo, RelationRetriever)): (QueryMemo, RelationRetriever) =
    {
      if (q in pairs) {
        println("Eliminated common subexpressions")
        (this, pairs(q))
      }
      else {
        println("Creating new subexpression")
        val (resMemo, r) = fallback
        val newMemo = new QueryMemo(this.pairs ++ resMemo.pairs + (q -> r), this.singles ++ resMemo.singles)
        (newMemo, r)
      }
    }

  def get(q: UnsafeFindSingle, fallback: => (QueryMemo, SingleRetriever)): (QueryMemo, SingleRetriever) = {
    if (q in singles) {
      println("Eliminated subexpression")
      (this, singles(q))
    }
    else {
      println("Creating new subexpression")
      val (resMemo, r) = fallback
      val newMemo = new QueryMemo(this.pairs ++ resMemo.pairs, this.singles ++ resMemo.singles + (q -> r))
      (newMemo, r)
    }
  }
}
