package impl.lmdb.common.tables.impl

import java.nio.ByteBuffer

import core.user.schema.TableName
import impl.lmdb.common.access.Key._
import impl.lmdb.common.access.{Commit, Key, ObjId}
import impl.lmdb.common.interfaces.LMDBInstance
import impl.lmdb.common.tables.interfaces.LMDBTable
import impl.lmdb.common.LMDBEither
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
    * Lookup a Set of commits
    * @param commits - commits to lookup
    * @return
    */
  def lookupSet(commits: List[Commit]): LMDBEither[Set[ObjId]] =
    getBatch[Set[ObjId], List](commits.map(getKey)).map(_.toSet.flatten)


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
