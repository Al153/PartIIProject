package impl.lmdb.common.tables.impl

import java.nio.ByteBuffer

import core.user.dsl.View
import impl.lmdb.common.access.Key._
import impl.lmdb.common.access.Storable.StorableView
import impl.lmdb.common.interfaces.LMDBInstance
import impl.lmdb.common.tables.interfaces.LMDBTable
import impl.lmdb.common.{LMDBFuture, _}
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
    logger.trace("Putting initial view")
    put(key, initialView)(StorableView, instance)

  }

  /**
    * Gets default view
    */
  def getDefault(): LMDBFuture[View] = LMDBFutureE(get[View](key)(instance, StorableView))

  /**
    * Sets default view, non transactional
    */
  def setDefault(v: View): LMDBFuture[Unit] = LMDBFutureE(put(key, v)(StorableView, instance))

}
