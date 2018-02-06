package impl.lmdb.cse.methods

import core.backend.intermediate.unsafe._
import impl.lmdb.common.LMDBEither
import impl.lmdb.common.access.{Commit, ObjId}
import impl.lmdb.cse.retrievers._

import scalaz._

/**
  * Created by Al on 06/02/2018.
  */
trait RetrieverMethods { self: Methods =>
  type Compilation[A] = State[QueryMemo, A]

  def get(q: UnsafeFindPair)(fallback: UnsafeFindPair => Compilation[RelationRetriever]): Compilation[RelationRetriever] = State[QueryMemo, RelationRetriever] {
    memo => memo.get(q, fallback(q).run(memo))
  }

  def get(q: UnsafeFindSingle)(fallback: UnsafeFindSingle => Compilation[SingleRetriever]): Compilation[SingleRetriever] = State[QueryMemo, SingleRetriever] {
    memo => memo.get(q, fallback(q).run(memo))
  }

  def getSingles(q: UnsafeFindSingle, commits: List[Commit]): LMDBEither[Set[ObjId]] =
    compileSingles(q, commits).run(emptyState)._2.find

  def getPairs(q: UnsafeFindPair, commits: List[Commit], from: Set[ObjId]): LMDBEither[Set[(ObjId, ObjId)]] =
    compilePairs(q, commits).run(emptyState)._2.find(from)

  def getFrom(q: UnsafeFindPair, commits: List[Commit]): (ObjId) => LMDBEither[Set[ObjId]] =
    compilePairs(q, commits).run(emptyState)._2.findRight(_)

  private def emptyState = new QueryMemo(Map(), Map())


  private def compileSingles(q: UnsafeFindSingle, commits: List[Commit]): Compilation[SingleRetriever] = get(q){
    case USFind(pattern) => State{ s => (s, new PatternRetriever(pattern, commits))}

    case USFrom(start, rel) => for {
      left <- compileSingles(start, commits)
      right <- compilePairs(rel, commits)
    } yield left into right

    case USAndS(left, right) => for {
      r1 <- compileSingles(left, commits)
      r2 <- compileSingles(right, commits)
    } yield r1 and r2

    case USOrS(left, right) => for {
      r1 <- compileSingles(left, commits)
      r2 <- compileSingles(right, commits)
    } yield r1 or r2
  }

  private def compilePairs(q: UnsafeFindPair, commits: List[Commit]): Compilation[RelationRetriever] =
    get(q) {
      case USAnd(l, r)  => for {
        a <- compilePairs(l, commits)
        b <- compilePairs(r, commits)
      } yield a and b

      case USAndRight(p, s) => for {
        pairRes <- compilePairs(p, commits)
        singleRes <- compileSingles(s, commits)
      } yield pairRes rightAnd singleRes

      case USAndLeft(p, s) => for {
        pairRes <- compilePairs(p, commits)
        singleRes <- compileSingles(s, commits)
      } yield pairRes leftAnd singleRes

      case USOr(l, r) => for {
        leftRes <- compilePairs(l, commits)
        rightRes <- compilePairs(r, commits)
      } yield leftRes or rightRes

      case USChain(l, r) => for {
        lres <- compilePairs(l, commits)
        rres <- compilePairs(r, commits) // reduce double joining
      } yield lres join rres

      case USDistinct(r) => for {
        rres <- compilePairs(r, commits)
      } yield rres.distinct

      case USId(_) => State(s => (s, IdRetriever))

      case USRel(rel) => State(s => (s, new RelRetriever(rel, commits)))

      case USRevRel(rel) => State(s => (s, new RevRelRetriever(rel, commits)))

      case USUpto(n, rel) =>
        for {
          r <- compilePairs(rel, commits)
        } yield r.upto(n)


      case USFixedPoint(rel) =>
        for {
          r <- compilePairs(rel, commits)
        } yield r.fixedPoint

      case USExactly(n, rel) =>
        for {
          r <- compilePairs(rel, commits)
        } yield r.exactly(n)
    }
}
