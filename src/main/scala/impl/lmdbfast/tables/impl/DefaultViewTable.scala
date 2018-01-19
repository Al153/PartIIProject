package impl.lmdbfast.tables.impl

import java.nio.ByteBuffer

import core.user.dsl.View
import impl.lmdbfast.access.Key._
import impl.lmdbfast.access.Storeable.StoreableView
import impl.lmdbfast.tables.interfaces.LMDBTable
import impl.lmdbfast.{LMDBFuture, LMDBInstance, _}
import org.lmdbjava.Dbi
import org.lmdbjava.DbiFlags._

/**
  * Created by Al on 28/12/2017.
  *
  * Holds the default view of the table
  */
class DefaultViewTable(implicit val instance: LMDBInstance) extends LMDBTable {
  import instance.executionContext

  override val db: Dbi[ByteBuffer] = instance.env.openDbi(name, MDB_CREATE)
  private val key = "default".key

  override def name: String =  "db:default"

  def initialise(): LMDBEither[Unit] = {
    println("Putting initial view")
    put(key, initialView)(StoreableView, instance)

  }

  /**
    * Gets default view
    */
  def getDefault(): LMDBFuture[View] = LMDBFutureE(get[View](key)(instance, StoreableView))

  /**
    * Sets default view, non transactional
    */
  def setDefault(v: View): LMDBFuture[Unit] = LMDBFutureE(put(key, v)(StoreableView, instance))

}
