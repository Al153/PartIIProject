package impl.lmdb.common.tables.impl

import java.nio.ByteBuffer

import impl.lmdb.common.interfaces.LMDBInstance
import impl.lmdb.common.tables.interfaces.RelationTable
import org.lmdbjava.Dbi
import org.lmdbjava.DbiFlags._

/**
  * Created by Al on 28/12/2017.
  *
  * Table containing all outgoing relations from objects, simply uses [[RelationTable]]'s code
  */
class ObjectRelations(implicit val instance: LMDBInstance) extends RelationTable {
  override def name: String = "db:outGoingRelations"
  override val db: Dbi[ByteBuffer] = instance.env.openDbi(name, MDB_CREATE)
}
