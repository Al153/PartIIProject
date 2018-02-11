package impl.lmdb.logjoins.retrievers

import core.utils._
import core.utils.algorithms.{FixedPointTraversal, SimpleFixedPointTraversal}
import impl.lmdb.common.LMDBEither
import impl.lmdb.common.access.ObjId
import impl.lmdb.common.errors.LMDBError
import impl.lmdb.logjoins._


trait RelationRetriever {
  def find(from: Set[ObjId]): LMDBEither[Map[ObjId, Set[ObjId]]]
  def findRight(from: ObjId): LMDBEither[Set[ObjId]]
  private val outer = this


  def join(that: RelationRetriever): RelationRetriever = new CachedRelationRetriever(
    objIds =>
      for {
        ls <- outer.find(objIds)
        middles = bigUnion(ls.values)
        rs <- that.find(middles)
        _ = logger.trace("Joining: left_size = " + ls.size + " Rights = " + rs.size)
      } yield ls.mapValues(s => s.flatMap(rs.getOrElse(_, Set()))).prune,
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
    } yield leftRes.mapValues{rightRes intersect _}.prune,
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
  def exactly(n: Int): RelationRetriever = {
      var i = n
      var acc = this
      var res: RelationRetriever = IdRetriever

      while (i > 0) {
        if ((i & 1) == 1) {
          res = res join acc
        }
        i = i >> 1
        acc = acc join acc
      }
      res
    }

  def upto(n: Int): RelationRetriever = {
    var i = n
    var acc = this
    var res: RelationRetriever = IdRetriever

    while (i > 0) {
      if ((i & 1) == 1) {
        res = res join acc
      }
      i = i >> 1
      acc = (acc join acc) or acc
    }
    res
  }
  def fixedPoint: RelationRetriever = new CachedRelationRetriever(
    objId =>
      FixedPointTraversal.fixedPoint[LMDBError, ObjId](_.flatMapE(outer.findRight), objId)
        .map(_.collectSets(identity)),
    SimpleFixedPointTraversal.fixedPoint(outer.findRight, _)
  )
}


















