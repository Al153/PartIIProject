package impl.lmdb.cse.retrievers
import core.backend.intermediate.unsafe._
import impl.lmdb.common.access.{Commit, ObjId}
import core.utils._
import impl.lmdb.common._
import impl.lmdb.common.interfaces.LMDBInstance

import scalaz._
import Scalaz._
/**
  * Class for doing lookups where no memo is faster (or better) than memo
  */
class UncachedRelationRetriever(
                                 lookup: Set[ObjId] => LMDBEither[Set[(ObjId, ObjId)]],
                                 simpleLookup: ObjId => LMDBEither[Set[ObjId]]
                               ) extends RelationRetriever {
                                 override def find(from: Set[ObjId]): LMDBEither[Set[(ObjId, ObjId)]] = lookup(from)
                                 override def findFrom(from: ObjId): LMDBEither[Set[ObjId]] = simpleLookup(from)
                               }
