package impl.lmdb.tables.impl

import java.nio.ByteBuffer

import impl.lmdb.LMDBInstance
import impl.lmdb.tables.interfaces.RelationTable
import org.lmdbjava.Dbi
import org.lmdbjava.DbiFlags._

/**
  * Created by Al on 28/12/2017.
  *
  * Table containing all incoming relations to objects, simply uses [[RelationTable]]'s code
  */
class ObjectReverseRelations (implicit val instance: LMDBInstance) extends RelationTable {
  override def name: String = "db:incomingRelations"
  override val db: Dbi[ByteBuffer] = instance.env.openDbi(name, MDB_CREATE)
}
