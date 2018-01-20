package impl.lmdbfast.tables.impl

import java.nio.ByteBuffer

import core.user.schema.TableName
import impl.lmdbfast.access.Key._
import impl.lmdbfast.access.{Commit, Key, ObjId}
import impl.lmdbfast.tables.interfaces.LMDBTable
import impl.lmdbfast.{LMDBEither, LMDBInstance}
import org.lmdbjava.Dbi
import org.lmdbjava.DbiFlags._

/**
  * Created by Al on 28/12/2017.
  *
  * Used to lookup all objects of the given type from a commit
  *
  * Essentially stores a Map[Commit, Set[ObjId] ]
  */
class EmptyIndexTable(tableName: TableName)(implicit val instance: LMDBInstance) extends LMDBTable {
  override val name: String = s"db:emptyIndex:$tableName"
  override val db: Dbi[ByteBuffer] = instance.env.openDbi(name, MDB_CREATE)

  /**
    * Append a commit to the path
    */
  private def getKey(commit: Commit): Key = commit.key

  /**
    * Lookup a commit
    * @param commit - commit to lookup
    * @return
    */
  private def lookup(commit: Commit): LMDBEither[Set[ObjId]] = get[Set[ObjId]](getKey(commit))

  /**
    * Lookup a Set of commits
    * @param commits - commits to lookup
    * @return
    */
  // todo: Set[Commit] => Array[Commit] for speed
  def lookupSet(commits: Set[Commit]): LMDBEither[Set[ObjId]] =
  getBatch[Set[ObjId], Set](commits.map(getKey)).map(_.flatten)


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
  def insert(commit: Commit, objId: ObjId): LMDBEither[Unit] = transactionalAppendToSet(getKey(commit), objId)

  /**
    * Do nothing to initialise
    * @return
    */
  override def initialise(): LMDBEither[Unit] = LMDBEither(())
}
