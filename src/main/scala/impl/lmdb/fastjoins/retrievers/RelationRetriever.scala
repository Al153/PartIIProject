package impl.lmdb.fastjoins.retrievers

import core.utils._
import core.utils.algorithms.{FixedPointTraversal, SimpleFixedPointTraversal}
import impl.lmdb.common.LMDBEither
import impl.lmdb.common.access.ObjId
import impl.lmdb.common.errors.LMDBError
import impl.lmdb.fastjoins._


trait RelationRetriever extends Logged {
  def find(from: Set[ObjId]): LMDBEither[Map[ObjId, Set[ObjId]]]
  def findFrom(from: ObjId): LMDBEither[Set[ObjId]]
}

object RelationRetriever {
  implicit class RelationRetrieverOps(outer: RelationRetriever) {
    def join(that: RelationRetriever): RelationRetriever = new CachedRelationRetriever(
      objIds =>
        for {
          ls <- outer.find(objIds)
          middles = bigUnion(ls.values)
          _ = logger.info("Middle size = " + middles.size)
          rs <- that.find(middles)
          _ = logger.info("Joining: left_size = " + ls.size + " Rights = " + rs.size)
        } yield ls.mapValues(s => s.flatMap(rs.getOrElse(_, Set()))).prune,
      outer.findFrom(_).flatMapS(that.findFrom)
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
              as.flatMapE(outer.findFrom)
            else
              LMDBEither(Set()))
    )
    def rightAnd(that: SingleRetriever): RelationRetriever = new CachedRelationRetriever(
      objIds => for {
        leftRes <- outer.find(objIds)
        rightRes <- that.find
      } yield leftRes.mapValues{rightRes intersect _}.prune,
      objId => for {
        rs <- outer.findFrom(objId)
        filter <- that.find
      } yield rs intersect filter
    )
    def distinct: RelationRetriever = new CachedRelationRetriever(
      objIds => for {
        rres <- outer.find(objIds)
      } yield rres.map{case (key, set) => key -> (set - key) }.prune,

      objId => for {
        res <- outer.findFrom(objId)
      } yield res - objId
    )
    def and(that: RelationRetriever): RelationRetriever = new CachedRelationRetriever(
      objIds => for {
        leftRes <- outer.find(objIds)
        rightRes <- that.find(objIds)
      } yield leftRes intersect rightRes,
      objId => for {
        rs <- outer.findFrom(objId)
        filter <- that.findFrom(objId)
      } yield rs intersect filter
    )
    def or(that: RelationRetriever): RelationRetriever = new CachedRelationRetriever(
      objIds => for {
        leftRes <- outer.find(objIds)
        rightRes <- that.find(objIds)
      } yield leftRes union rightRes,
      objId => for {
        rs <- outer.findFrom(objId)
        filter <- that.findFrom(objId)
      } yield rs union filter
    )

    def exactly(n: Int): RelationRetriever = repeat(n, outer, IdRetriever)

    def upto(n: Int): RelationRetriever = repeat(n, outer or IdRetriever, IdRetriever)

    private def repeat(n: Int, acc: RelationRetriever, res: RelationRetriever): RelationRetriever = if (n > 0) {
      val newRes = if((n & 1) == 1)  res join acc else res
      val newAcc = acc join acc
      repeat(n >> 1, newAcc, newRes)
    } else res

    def fixedPoint: RelationRetriever = new CachedRelationRetriever(
      objId =>
        FixedPointTraversal.fixedPoint[LMDBError, ObjId](_.flatMapE(outer.findFrom), objId)
          .map(_.collectSets(identity)),
      SimpleFixedPointTraversal.fixedPoint(outer.findFrom, _)
    )
  }
}


















