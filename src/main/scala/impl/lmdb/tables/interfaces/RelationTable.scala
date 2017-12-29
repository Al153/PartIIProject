package impl.lmdb.tables.interfaces

import core.schema.RelationName
import impl.lmdb.access.{Commit, ObjId}
import impl.lmdb.containers.{Extractor, Inserter}
import impl.lmdb.retrieval.RelationDestination

/**
  * Created by Al on 29/12/2017.
  */
trait RelationTable extends LMDBTable {
  def lookup(objId: ObjId): Extractor[RelationDestination] = ???
  def lookup(objId: ObjId, commit: Commit): Extractor[RelationDestination] = ???
  def followRelation(objId: ObjId, commit: Commit, relation: RelationName): Extractor[ObjId] =
    lookup(objId, commit) collect {
      case RelationDestination(relationName, _, objectId) if relationName == relation => objectId
    }

  def insert(from: ObjId, commit: Commit, relationName: RelationName, to: ObjId): Inserter[RelationDestination] = ???
}
