package impl.lmdb.cse.retrievers

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



trait RelationRetriever extends Logged {
  def find(from: Set[ObjId]): LMDBEither[Set[(ObjId, ObjId)]]
  def findRight(from: ObjId): LMDBEither[Set[ObjId]]
  private val outer = this

  def join(that: RelationRetriever): RelationRetriever = new CachedRelationRetriever(
    objIds =>
      for {
        ls <- outer.find(objIds)
        rs <- that.find(ls.mapProj2)
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


















