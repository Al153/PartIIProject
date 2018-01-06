package impl.lmdb.tables.impl

import core.backend.common.DBCell
import core.backend.intermediate.SchemaComponent
import core.user.schema.TableName
import core.utils.EitherOps
import impl.lmdb.access.Key._
import impl.lmdb.access.{Commit, Key, ObjId}
import impl.lmdb.tables.interfaces.LMDBTable
import impl.lmdb.{LMDBEither, LMDBInstance}
import impl.lmdb._

/**
  * Created by Al on 28/12/2017.
  *
  * table used to index into the DB
  */

class ColumnIndexTable(tableName: TableName, columnIndex: Int, expectedType: SchemaComponent)(implicit val instance: LMDBInstance) extends LMDBTable {
  override def path: Key = tableName >> columnIndex

  def lookup(value: DBCell, commit: Commit): LMDBEither[Set[ObjId]] = get(getKey(value, commit))
  def lookup(value: DBCell, commits: Set[Commit]): LMDBEither[Set[ObjId]] =
    EitherOps.sequence(commits.map(
      commit => lookup(value, commit)
    )).map(_.flatten)

  def insert(value: DBCell, commit: Commit, o: ObjId): LMDBEither[Unit] =
    transactionalAppendToSet(getKey(value, commit), o)

  def getKey(value: DBCell, commit: Commit): Key = path >> value >> commit
}

object ColumnIndexTable {

}