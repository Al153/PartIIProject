package impl.lmdb.tables.impl

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
  * Used to lookup all objects of the given type from a commit
  *
  * Essentially stores a Map[Commit, Set[ObjId] ]
  */
class EmptyIndexTable(tableName: TableName)(implicit val instance: LMDBInstance) extends LMDBTable {
  override val path: Key = "db" >> "emptyIndex" >> tableName

  /**
    * Append a commit to the path
    */
  private def getPath(commit: Commit): Key = path >> commit

  /**
    * Lookup a commit
    * @param commit - commit to lookup
    * @return
    */
  private def lookup(commit: Commit): LMDBEither[Set[ObjId]] = get[Set[ObjId]](getPath(commit))

  /**
    * Lookup a Set of commits
    * @param commits - commits to lookup
    * @return
    */
  // todo: Set[Commit] => Array[Commit] for speed
  def lookupSet(commits: Set[Commit]): LMDBEither[Set[ObjId]] = {
    for {
      sets <- EitherOps.sequence(commits.map(lookup))
    } yield sets.flatten
  }

  /**
    * Lookup commits and return as a vector
    * @param commits - commits to lookup
    * @return
    */
  // todo: this could be implemented at a lower level
  def lookupVector(commits: Set[Commit]): LMDBEither[Vector[ObjId]] = lookupSet(commits).map(_.toVector)

  /**
    * Insert an object
    */
  // todo: do batching like change to [[ColumnIndexTable]]
  def insert(commit: Commit, objId: ObjId): LMDBEither[Unit] = transactionalAppendToSet(getPath(commit), objId)

  /**
    * Do nothing to initialise
    * @return
    */
  override def initialise(): LMDBEither[Unit] = LMDBEither(())
}
