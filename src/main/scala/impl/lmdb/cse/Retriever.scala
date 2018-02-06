package impl.lmdb.cse

import core.backend.intermediate.unsafe._
import core.utils._
import core.utils.algorithms.{FixedPointTraversal, Joins, SimpleFixedPointTraversal}
import impl.lmdb.common.LMDBEither
import impl.lmdb.common.access.{Commit, ObjId}
import impl.lmdb.common.errors.LMDBError
import impl.lmdb.common.interfaces.LMDBInstance

import scala.collection.mutable
import scalaz.Scalaz._
import scalaz._



trait RelationRetriever {
  def find(from: Set[ObjId]): LMDBEither[Set[(ObjId, ObjId)]]
  def findRight(from: ObjId): LMDBEither[Set[ObjId]]
  private val outer = this

  def join(that: RelationRetriever): RelationRetriever = new CachedRelationRetriever(
    objIds =>
      for {
        ls <- outer.find(objIds)
        rs <- that.find(objIds)
      } yield Joins.joinSet(ls, rs),
    outer.findRight(_).flatMapS(that.findRight)
  )
  def leftAnd(that: SingleRetriever): RelationRetriever = new CachedRelationRetriever(
    objIds =>
      for {
        rightRes <- that.find
        pairRes <- outer.find(rightRes intersect objIds)
      } yield pairRes,
    objId =>
      that.find.flatMap(
        as =>
          if (objId in as)
            as.flatMapE(outer.findRight)
          else
            LMDBEither(Set()))
  )
  def rightAnd(that: SingleRetriever): RelationRetriever = new CachedRelationRetriever(
    objIds => for {
      leftRes <- outer.find(objIds)
      rightRes <- that.find
    } yield leftRes.filter{case (_, b) => rightRes.contains(b)},
    objId => for {
      rs <- outer.findRight(objId)
      filter <- that.find
    } yield rs intersect filter
  )
  def distinct: RelationRetriever = new CachedRelationRetriever(
    objIds => for {
      rres <- outer.find(objIds)
    } yield rres.filter{case (a, b) => a != b},
    objId => for {
      res <- outer.findRight(objId)
    } yield res - objId
  )
  def and(that: RelationRetriever): RelationRetriever = new CachedRelationRetriever(
    objIds => for {
      leftRes <- outer.find(objIds)
      rightRes <- that.find(objIds)
    } yield leftRes intersect rightRes,
    objId => for {
      rs <- outer.findRight(objId)
      filter <- that.findRight(objId)
    } yield rs intersect filter
  )
  def or(that: RelationRetriever): RelationRetriever = new CachedRelationRetriever(
    objIds => for {
      leftRes <- outer.find(objIds)
      rightRes <- that.find(objIds)
    } yield leftRes union rightRes,
    objId => for {
      rs <- outer.findRight(objId)
      filter <- that.findRight(objId)
    } yield rs union filter
  )
  def exactly(n: Int): RelationRetriever = new CachedRelationRetriever(
    FixedPointTraversal.exactly(outer.findRight, _, n),
    SimpleFixedPointTraversal.exactly(outer.findRight, _, n)
  )
  def upto(n: Int): RelationRetriever = new CachedRelationRetriever(
    FixedPointTraversal.upTo(outer.findRight, _, n),
    SimpleFixedPointTraversal.upTo(outer.findRight, _, n)
  )
  def fixedPoint: RelationRetriever = new CachedRelationRetriever(
    objId => FixedPointTraversal.fixedPoint[LMDBError, ObjId](_.flatMapE(outer.findRight), objId),
    SimpleFixedPointTraversal.fixedPoint(outer.findRight, _)
  )
}

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

/**
  * Class for doing lookups where no memo is faster (or better) than memo
  */
class UncachedRelationRetriever(
                                 lookup: Set[ObjId] => LMDBEither[Set[(ObjId, ObjId)]],
                                 simpleLookup: ObjId => LMDBEither[Set[ObjId]]
                               ) extends RelationRetriever {
  override def find(from: Set[ObjId]): LMDBEither[Set[(ObjId, ObjId)]] = lookup(from)
  override def findRight(from: ObjId): LMDBEither[Set[ObjId]] = simpleLookup(from)
}

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

class CachedSingleRetriever(lookup: => LMDBEither[Set[ObjId]]) extends SingleRetriever {
  override lazy val find: LMDBEither[Set[ObjId]] = lookup
}

class UncachedSingleRetriever(lookup: => LMDBEither[Set[ObjId]]) extends SingleRetriever {
  override def find: LMDBEither[Set[ObjId]] = lookup
}

object Retrievers {
  val instance: LMDBInstance = ???

  object IdRetriever extends UncachedRelationRetriever(
    objIds => objIds.mapPair.right,
    objId => Set(objId).right
  )

  class RelRetriever(r: ErasedRelationAttributes, commits: List[Commit]) extends UncachedRelationRetriever(
    objIds =>
      objIds.flatMapE {
        leftObject => for {
          related <- instance.controlTables.relations.followRelation(leftObject, commits, r.name)
        } yield related.map((leftObject, _))
      },
    objId => instance.controlTables.relations.followRelation(objId, commits, r.name)

  )

  class RevRelRetriever(r: ErasedRelationAttributes, commits: List[Commit]) extends UncachedRelationRetriever(
    objIds =>
      objIds.flatMapE {
        leftObject => for {
          related <- instance.controlTables.reverseRelations.followRelation(leftObject, commits, r.name)
        } yield related.map((leftObject, _))
      },
    objId => instance.controlTables.reverseRelations.followRelation(objId, commits, r.name)
  )

  class PatternRetriever(pattern: ErasedFindable, commits: List[Commit]) extends UncachedSingleRetriever(
     instance.lookupPattern(pattern, commits)
  )
}




object CachedRetriever {
  import Retrievers._
  type Compilation[A] = State[QueryMemo, A]

  def get(q: UnsafeFindPair)(fallback: UnsafeFindPair => Compilation[RelationRetriever]): Compilation[RelationRetriever] = State[QueryMemo, RelationRetriever] {
    memo => memo.get(q, fallback(q).run(memo))
  }

  def get(q: UnsafeFindSingle)(fallback: UnsafeFindSingle => Compilation[SingleRetriever]): Compilation[SingleRetriever] = State[QueryMemo, SingleRetriever] {
    memo => memo.get(q, fallback(q).run(memo))
  }


  def getSingles(q: UnsafeFindSingle, commits: List[Commit]): Compilation[SingleRetriever] = get(q){
    case USFind(pattern) => State{ s => (s, new PatternRetriever(pattern, commits))}

    case USFrom(start, rel) => for {
      left <- getSingles(start, commits)
      right <- getPairs(rel, commits)
    } yield left into right

    case USAndS(left, right) => for {
      r1 <- getSingles(left, commits)
      r2 <- getSingles(right, commits)
    } yield r1 and r2

    case USOrS(left, right) => for {
      r1 <- getSingles(left, commits)
      r2 <- getSingles(right, commits)
    } yield r1 or r2
  }

  def getPairs(q: UnsafeFindPair, commits: List[Commit]): Compilation[RelationRetriever] =
    get(q) {
      case USAnd(l, r)  => for {
        a <- getPairs(l, commits)
        b <- getPairs(r, commits)
      } yield a and b

      case USAndRight(p, s) => for {
        pairRes <- getPairs(p, commits)
        singleRes <- getSingles(s, commits)
      } yield pairRes rightAnd singleRes

      case USAndLeft(p, s) => for {
        pairRes <- getPairs(p, commits)
        singleRes <- getSingles(s, commits)
      } yield pairRes leftAnd singleRes

      case USOr(l, r) => for {
        leftRes <- getPairs(l, commits)
        rightRes <- getPairs(r, commits)
      } yield leftRes or rightRes

      case USChain(l, r) => for {
        lres <- getPairs(l, commits)
        rres <- getPairs(r, commits) // reduce double joining
      } yield lres join rres

      case USDistinct(r) => for {
        rres <- getPairs(r, commits)
      } yield rres.distinct

      case USId(_) => State(s => (s, IdRetriever))

      case USRel(rel) => State(s => (s, new RelRetriever(rel, commits)))

      case USRevRel(rel) => State(s => (s, new RevRelRetriever(rel, commits)))

      case USUpto(n, rel) =>
        for {
          r <- getPairs(rel, commits)
        } yield r.upto(n)


      case USFixedPoint(rel) =>
        for {
          r <- getPairs(rel, commits)
        } yield r.fixedPoint

      case USExactly(n, rel) =>
        for {
          r <- getPairs(rel, commits)
        } yield r.exactly(n)
    }
}

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