package impl.lmdb.tables.impl

import impl.lmdb.LMDBInstance
import impl.lmdb.access.Key._
import impl.lmdb.access.ObjId
import impl.lmdb.tables.interfaces.MutableCounter
import org.fusesource.lmdbjni.Database

import scala.language.postfixOps

/**
  * Created by Al on 29/12/2017.
  *
  * Simple mutable counter for creating new ObjIds
  */
class ObjectsCounter(implicit val instance: LMDBInstance) extends MutableCounter[ObjId]("Objects".key) {
  override def name: String = "db:nextObject"
  override val db: Database = instance.env.openDatabase(name)
  override val initialValue = ObjId(0)



  /**
    * Simply increment the Id
    * @param a - old value
    * @return
    */
  override def next(a: ObjId): ObjId = a.increment
}
