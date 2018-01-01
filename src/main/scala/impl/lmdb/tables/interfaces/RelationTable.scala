package impl.lmdb.tables.interfaces

import core.schema.RelationName
import impl.lmdb.LMDBEither
import impl.lmdb.access.{Commit, ObjId}

/**
  * Created by Al on 29/12/2017.
  */
trait RelationTable extends LMDBTable {

  def followRelation(objId: ObjId, commits: Set[Commit], relation: RelationName): LMDBEither[Set[ObjId]] = ???

  def insert(from: ObjId, commit: Commit, relationName: RelationName, to: Set[ObjId]): LMDBEither[Unit] = ???
}
