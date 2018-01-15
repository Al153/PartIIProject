package impl.lmdb.tables.impl

import impl.lmdb.LMDBInstance
import impl.lmdb.tables.interfaces.RelationTable
import org.fusesource.lmdbjni.Database

/**
  * Created by Al on 28/12/2017.
  *
  * Table containing all outgoing relations from objects, simply uses [[RelationTable]]'s code
  */
class ObjectRelations(implicit val instance: LMDBInstance) extends RelationTable {
  override def name: String = "db:outGoingRelations"
  override val db: Database = instance.env.openDatabase(name)
}
