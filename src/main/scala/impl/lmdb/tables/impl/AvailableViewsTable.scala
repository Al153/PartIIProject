package impl.lmdb.tables.impl

import java.nio.ByteBuffer

import core.user.dsl.View
import core.utils._
import impl.lmdb
import impl.lmdb.access.Key._
import impl.lmdb.errors.InvalidView
import impl.lmdb.tables.interfaces.LMDBTable
import impl.lmdb.{LMDBEither, LMDBInstance, _}
import org.lmdbjava.Dbi
import org.lmdbjava.DbiFlags.MDB_CREATE

import scalaz.Scalaz._
import scalaz._

/**
  * Created by Al on 28/12/2017.
  *
  * A table containing the views that are currently valid
  */
class AvailableViewsTable(implicit val instance: LMDBInstance) extends LMDBTable {
  override val db: Dbi[ByteBuffer] = instance.env.openDbi(name, MDB_CREATE)
  private val key = "available".key

  // todo: have sub table containing a logical clock, allowing us to store an easily validated cache
  override def name: String = "db:views"

  /**
    * To initialise, simply set the initial view
    * @return
    */
  override def initialise(): LMDBEither[Unit] = put[Set[View]](key, Set(initialView))

  /**
    * Get available views in DB
    * @return
    */
  def availableViews(): LMDBEither[Set[View]] = get(key)

  /**
    * Check a view is available
    */
  def validateView(v: View): LMDBEither[Unit] = for {
    views <- availableViews()
    _ <- if (v in views) LMDBEither(()) else InvalidView(v).left
  } yield ()

  /**
    * Append a view to the available views
    */
  def insertNewView(v: View): LMDBEither[Unit] = transactionalAppendToSet(key, v)

  // todo: optimise by storing the underlying hashtable of the set in LMDB
}
