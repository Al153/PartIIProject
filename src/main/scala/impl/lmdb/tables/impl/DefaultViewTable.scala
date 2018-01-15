package impl.lmdb.tables.impl

import core.user.dsl.View
import impl.lmdb.access.Key
import impl.lmdb.access.Key._
import impl.lmdb.tables.interfaces.LMDBTable
import impl.lmdb.{LMDBFuture, LMDBInstance}
import impl.lmdb._
import impl.lmdb.access.Storeable.StoreableView
import org.fusesource.lmdbjni.Database

/**
  * Created by Al on 28/12/2017.
  *
  * Holds the default view of the table
  */
class DefaultViewTable(implicit val instance: LMDBInstance) extends LMDBTable {
  import instance.executionContext

  override val db: Database = instance.env.openDatabase(name)
  private val key = "default".key

  override def name: String =  "db:default"

  def initialise(): LMDBEither[Unit] = put(key, initialView, db)(StoreableView)

  /**
    * Gets default view
    */
  def getDefault(): LMDBFuture[View] = LMDBFutureE(get[View](db, key)(StoreableView))

  /**
    * Sets default view, non transactional
    */
  def setDefault(v: View): LMDBFuture[Unit] = LMDBFutureE(put(key, v, db)(StoreableView))

}
