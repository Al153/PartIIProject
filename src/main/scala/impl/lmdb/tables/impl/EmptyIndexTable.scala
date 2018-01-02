package impl.lmdb.tables.impl

import core.schema.TableName
import core.utils.EitherOps
import impl.lmdb.access.Key._
import impl.lmdb.access.{Commit, Key, ObjId}
import impl.lmdb.tables.interfaces.LMDBTable
import impl.lmdb.{LMDBEither, LMDBInstance}
import impl.lmdb._

/**
  * Created by Al on 28/12/2017.
  *
  * Used to lookup all objects of a type from a commit
  */
class EmptyIndexTable(tableName: TableName)(implicit val instance: LMDBInstance) extends LMDBTable {
  override val path: Key = "db" >> "emptyIndex" >> tableName
  def getPath(commit: Commit): Key = path >> commit

  private def lookup(commit: Commit): LMDBEither[Set[ObjId]] = get(getPath(commit))
  def lookupSet(commits: Set[Commit]): LMDBEither[Set[ObjId]] = for {
    sets <- EitherOps.sequence(commits.map(lookup))
  } yield sets.flatten

  def lookupVector(commits: Set[Commit]): LMDBEither[Vector[ObjId]] = lookupSet(commits).map(_.toVector)

  def insert(commit: Commit, objId: ObjId): LMDBEither[Unit] = transactionalAppendToSet(getPath(commit), objId)
}
