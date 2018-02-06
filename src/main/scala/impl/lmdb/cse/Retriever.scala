import core.backend.intermediate.unsafe.UnsafeFindPair
import impl.lmdb.common.LMDBEither
import impl.lmdb.common.access.{Commit, ObjId}

/**
  * Trait wraps around a fn to cache the result
  *
  * @tparam A
  * @tparam B
  */
trait Retriever[A, B] {
  def apply(a: A): B
}

class CachedRetriever[A, B](f: A => B, limit: Int) extends Retriever[A, B]{
  private val state = new java.util.concurrent.ConcurrentHashMap[A, B]()
  private val entries = new java.util.concurrent.ConcurrentSkipListSet[A]()
  override def apply(a: A): B = {
    state.computeIfAbsent(a, a1 => f(a1))
  }
}

class RelationRetriever[A, B, Commit](lookup: (Commit, Set[A]) => LMDBEither[Set[(A, B)]]) extends Retriever [(Commit, Set[A]), LMDBEither[Set[(A, B)]]] {
  override def apply(a: (Commit, Set[A])): LMDBEither[Set[(A, B)]] = ???
}
/*
object CachedRetriever {
  def getPairs(q: UnsafeFindPair): CachedRetriever[List[Commit], LMDBEither[Set[(ObjId, ObjId)]]] =
    q match {
      case
    }
}
*/

class SubexprTable[A, B](m: Map[UnsafeFindPair, CachedRetriever[A, B]])