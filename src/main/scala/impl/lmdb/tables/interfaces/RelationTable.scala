package impl.lmdb.tables.interfaces

import core.schema.RelationName
import core.utils.EitherOps
import impl.lmdb.LMDBEither
import impl.lmdb.access.{Commit, Key, ObjId}
import impl.lmdb._
import Key._

/**
  * Created by Al on 29/12/2017.
  */
trait RelationTable extends LMDBTable {

  def getKey(objId: ObjId, commit: Commit, relationName: RelationName): Key =
    path >> objId >> commit >> relationName

  def followRelation(objId: ObjId, commits: Set[Commit], relation: RelationName): LMDBEither[Set[ObjId]] =
    EitherOps.sequence(
      commits.map {
        commit =>
          get[Set[ObjId]](getKey(objId, commit, relation))
      }
    ).map(_.flatten)

  def insert(from: ObjId, commit: Commit, relationName: RelationName, to: Set[ObjId]): LMDBEither[Unit] =
      transactionalUnion(getKey(from, commit, relationName), to)

}
