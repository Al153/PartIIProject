package impl.lmdb.tables.impl

import core.backend.common.DBCell
import core.schema.{SchemaComponent, TableName}
import impl.lmdb.LMDBInstance
import impl.lmdb.access.Key._
import impl.lmdb.access.{Commit, Key, ObjId}
import impl.lmdb.containers.Extractor
import impl.lmdb.tables.interfaces.LMDBTable

/**
  * Created by Al on 28/12/2017.
  *
  * table used to index into the DB
  */

class ColumnIndexTable(tableName: TableName, columnIndex: Int, expectedType: SchemaComponent)(implicit val instance: LMDBInstance) extends LMDBTable {
  override def path: Key = tableName.key :: columnIndex.key
  def lookup(value: DBCell, commit: Commit): Extractor[ObjId] = ???
}

object ColumnIndexTable {

}