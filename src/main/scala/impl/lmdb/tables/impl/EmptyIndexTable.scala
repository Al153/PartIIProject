package impl.lmdb.tables.impl

import core.schema.TableName
import impl.lmdb.LMDBInstance
import impl.lmdb.access.Key._
import impl.lmdb.access.{Commit, Key, ObjId}
import impl.lmdb.containers.Extractor
import impl.lmdb.tables.interfaces.LMDBTable

/**
  * Created by Al on 28/12/2017.
  *
  * Used to lookup all objects of a type from a commit
  */
class EmptyIndexTable(tableName: TableName)(implicit val instance: LMDBInstance) extends LMDBTable {
  override def path: Key = "db".key :: "emptyIndex".key :: tableName.key

  def lookup(commit: Commit): Extractor[ObjId] = ???
}
