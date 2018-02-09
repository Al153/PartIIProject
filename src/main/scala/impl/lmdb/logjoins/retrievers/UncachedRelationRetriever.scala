package impl.lmdb.logjoins.retrievers

import impl.lmdb.common._
import impl.lmdb.common.access.ObjId

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
