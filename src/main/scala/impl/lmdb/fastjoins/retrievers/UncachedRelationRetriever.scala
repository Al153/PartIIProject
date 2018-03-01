package impl.lmdb.fastjoins.retrievers

import impl.lmdb.common._
import impl.lmdb.common.access.ObjId
import core.utils._

/**
  * Class for doing lookups where no memo is faster (or better) than memo
  */
class UncachedRelationRetriever(
                                 lookup: Set[ObjId] => LMDBEither[Set[(ObjId, ObjId)]],
                                 simpleLookup: ObjId => LMDBEither[Set[ObjId]]
                               ) extends RelationRetriever {
                                 override def find(from: Set[ObjId]): LMDBEither[Map[ObjId, Set[ObjId]]] = lookup(from).map(_.collectSets(identity))
                                 override def findFrom(from: ObjId): LMDBEither[Set[ObjId]] = simpleLookup(from)
                               }
