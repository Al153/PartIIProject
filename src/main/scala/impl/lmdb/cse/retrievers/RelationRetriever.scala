package impl.lmdb.cse.retrievers

import core.utils._
import core.utils.algorithms.{FixedPointTraversal, Joins, SimpleFixedPointTraversal}
import impl.lmdb.common.LMDBEither
import impl.lmdb.common.access.ObjId
import impl.lmdb.common.errors.LMDBError


/**
  * A relation retriever is a wrapper around a pair of lookup functions
  */
trait RelationRetriever extends Logged {
  /**
    * Find the pairs from an initial set
    * @param from - Filter for lhs values
    * @return
    */
  def find(from: Set[ObjId]): LMDBEither[Set[(ObjId, ObjId)]]

  /**
    * Lookup all reachable values from an initial value
    */
  def findRight(from: ObjId): LMDBEither[Set[ObjId]]
}

object RelationRetriever {

  /**
    * Operations on Retrievers
    */
  implicit class RetrieverOps(outer: RelationRetriever) {
    /**
      *
      * Joins a pair of retrievers
      * @return
      */
    def join(that: RelationRetriever): RelationRetriever =
      // do a Memory Implementation style join
      new CachedRelationRetriever(
        objIds =>
          for {
            ls <- outer.find(objIds)
            rs <- that.find(ls.mapProj2)
          } yield Joins.joinSet(ls, rs),
        // join reduced to a flatmap
        outer.findRight(_).flatMapS(that.findRight)
    )

    /**
      * solve a [[core.backend.intermediate.unsafe.USAndLeft]]
      */
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
    /**
      * solve a [[core.backend.intermediate.unsafe.USAndRight]]
      */
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

    /**
      * solve a [[core.backend.intermediate.unsafe.USDistinct]]
      * @return
      */
    def distinct: RelationRetriever = new CachedRelationRetriever(
      objIds => for {
        rres <- outer.find(objIds)
      } yield rres.filter{case (a, b) => a != b},

      objId => for {
        res <- outer.findRight(objId)
      } yield res - objId
    )

    /**
      * solve a [[core.backend.intermediate.unsafe.USAnd]]
      */
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
    /**
      * solve a [[core.backend.intermediate.unsafe.USOr]]
      */
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

    /**
      * Thread a result through the Exactly algorithm
      * @param n
      * @return
      */
    def exactly(n: Int): RelationRetriever = new CachedRelationRetriever(
      FixedPointTraversal.exactly(outer.findRight, _, n),
      SimpleFixedPointTraversal.exactly(outer.findRight, _, n)
    )

    /**
      * Thread through the upto algorithm
      * @param n
      * @return
      */
    def upto(n: Int): RelationRetriever = new CachedRelationRetriever(
      FixedPointTraversal.upTo(outer.findRight, _, n),
      SimpleFixedPointTraversal.upTo(outer.findRight, _, n)
    )

    /**
      * Thread through a fixed point
      * @return
      */
    def fixedPoint: RelationRetriever = new CachedRelationRetriever(
      objId => FixedPointTraversal.fixedPoint[LMDBError, ObjId](_.flatMapE(outer.findRight), objId),
      SimpleFixedPointTraversal.fixedPoint(outer.findRight, _)
    )
  }



}


















