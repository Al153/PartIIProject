package impl.lmdb.tables.impl

import impl.lmdb.LMDBInstance
import impl.lmdb.access.Key
import impl.lmdb.access.Key._
import impl.lmdb.tables.interfaces.RelationTable
import org.fusesource.lmdbjni.Database

/**
  * Created by Al on 28/12/2017.
  *
  * Table containing all incoming relations to objects, simply uses [[RelationTable]]'s code
  */
class ObjectReverseRelations (implicit val instance: LMDBInstance) extends RelationTable {
  override def name: String = "db:incomingRelations"
  override val db: Database = instance.env.openDatabase(name)
}
