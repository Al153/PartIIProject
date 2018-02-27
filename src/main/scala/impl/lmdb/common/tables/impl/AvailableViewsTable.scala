package impl.lmdb.common.tables.impl

import java.nio.ByteBuffer

import core.user.dsl.ViewId
import core.utils._
import impl.lmdb.common
import impl.lmdb.common.access.Key._
import impl.lmdb.common.errors.InvalidView
import impl.lmdb.common.tables.interfaces.{LMDBTable, MutableCounter}
import impl.lmdb.common.LMDBEither
import impl.lmdb.common.interfaces.LMDBInstance
import org.lmdbjava.Dbi
import org.lmdbjava.DbiFlags.MDB_CREATE
import impl.lmdb.common._

import scalaz.Scalaz._
import scalaz._

/**
  * Created by Al on 28/12/2017.
  *
  * A table containing the views that are currently valid.
  */
class AvailableViewsTable(implicit val instance: LMDBInstance) extends LMDBTable {
  override val db: Dbi[ByteBuffer] = instance.env.openDbi(name, MDB_CREATE)
  private val key = "available".key

  override def name: String = "db:views"

  /**
    * To initialise, simply set the initial view
    * @return
    */
  override def initialise(): LMDBEither[Unit] = put[Set[ViewId]](key, Set(initialView))

  /**
    * Get available views in DB
    * @return
    */
  def availableViews(): LMDBEither[Set[ViewId]] = get(key)

  /**
    * Check a view is available
    */
  // check logical clock. If valid, test against local set
  def validateView(v: ViewId): LMDBEither[Unit] = for {
    views <- availableViews()
    _ <- if (v in views) LMDBEither(()) else InvalidView(v).left
  } yield ()

  /**
    * Append a view to the available views
    */

  def insertNewView(v: ViewId): LMDBEither[Unit] = transactionalAppendToSet(key, v)

}