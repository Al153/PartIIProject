package impl.lmdb.tables.impl

import java.nio.ByteBuffer

import core.backend.common.DBCell
import core.backend.intermediate.SchemaComponent
import core.user.schema.TableName
import core.utils.EitherOps
import impl.lmdb.access.Key._
import impl.lmdb.access.{Commit, Key, ObjId}
import impl.lmdb.tables.interfaces.LMDBTable
import impl.lmdb.{LMDBEither, LMDBInstance, _}
import org.lmdbjava.Dbi
import org.lmdbjava.DbiFlags._

/**
  * Created by Al on 28/12/2017.
  *
  * table used to index into the DB for a given object type
  */

// todo optimise inserts using transactions (ie store valeus to insert in memory, then insert at once at the end of the transaction
class ColumnIndexTable(tableName: TableName, columnIndex: Int, expectedType: SchemaComponent)(implicit val instance: LMDBInstance) extends LMDBTable {
  override val db: Dbi[ByteBuffer] = instance.env.openDbi(name, MDB_CREATE)

  override def name: String = s"db:$tableName:$columnIndex"

  /**
    * Lookup a single commit + DBCell
    */
  private def lookup(value: DBCell, commit: Commit): LMDBEither[Set[ObjId]] = get(getKey(value, commit))

  /**
    * Lookup a set of commits + DBCell
    */
  def lookup(value: DBCell, commits: Set[Commit]): LMDBEither[Set[ObjId]] =
    EitherOps.sequence(commits.map(
      commit => lookup(value, commit)
    )).map(_.flatten)

  /**
    * Insert an object into a commit
    * @return
    */
  def insert(value: DBCell, commit: Commit, o: ObjId): LMDBEither[Unit] =
    transactionalAppendToSet(getKey(value, commit), o)

  /**
    * Convert a commit and DBCell into a key
    */
  private def getKey(value: DBCell, commit: Commit): Key = value >> commit

  /** No initialisation needed
   */
  override def initialise(): LMDBEither[Unit] = LMDBEither(())
}