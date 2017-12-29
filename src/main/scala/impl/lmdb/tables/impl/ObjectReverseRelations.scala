package impl.lmdb.tables.impl

import impl.lmdb.LMDBInstance
import impl.lmdb.access.Key
import impl.lmdb.access.Key._
import impl.lmdb.tables.interfaces.RelationTable

/**
  * Created by Al on 28/12/2017.
  */
class ObjectReverseRelations (implicit val instance: LMDBInstance) extends RelationTable {
  override val path: Key = "db".key :: "incomingRelations".key
}
