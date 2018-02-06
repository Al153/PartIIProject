package impl.lmdb.cse.retrievers
import core.backend.intermediate.unsafe._
import core.utils._

/**
  * Created by Al on 06/02/2018.
  */
class QueryMemo(val pairs: Map[UnsafeFindPair, RelationRetriever], val singles: Map[UnsafeFindSingle, SingleRetriever]) {
  def get(q: UnsafeFindPair, fallback: => (QueryMemo, RelationRetriever)): (QueryMemo, RelationRetriever) =
    if (q in pairs) (this, pairs(q))
    else {
      val (resMemo, r) = fallback
      val newMemo = new QueryMemo(this.pairs ++ resMemo.pairs + (q -> r), this.singles ++ resMemo.singles)
      (newMemo, r)
    }

  def get(q: UnsafeFindSingle, fallback: => (QueryMemo, SingleRetriever)): (QueryMemo, SingleRetriever) = {
    if (q in singles) (this, singles(q))
    else {
      val (resMemo, r) = fallback
      val newMemo = new QueryMemo(this.pairs ++ resMemo.pairs, this.singles ++ resMemo.singles + (q -> r))
      (newMemo, r)
    }
  }
}
