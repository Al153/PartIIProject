package impl.lmdb.common.tables.impl

import java.nio.ByteBuffer

import core.user.dsl.View
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
  *
  * This is upgraded from the lmdb implementation in that we cache the views tables
  */
class AvailableViewsTable(implicit val instance: LMDBInstance) extends LMDBTable {
  override val db: Dbi[ByteBuffer] = instance.env.openDbi(name, MDB_CREATE)
  private val key = "available".key
  private var logicalClockValue = -1 // initial value = invalid

  // todo: Make use of the underlying subtable for the logical clock
  override def name: String = "db:views"

  /**
    * To initialise, simply set the initial view
    * @return
    */
  // todo: initialise logical clock
  override def initialise(): LMDBEither[Unit] = put[Set[View]](key, Set(initialView))

  /**
    * Get available views in DB
    * @return
    */
  // todo: check clock value against own value, if invalid, get from database, otherwise
  def availableViews(): LMDBEither[Set[View]] = get(key)

  /**
    * Check a view is available
    */
  // check logical clock. If valid, test against local set
  def validateView(v: View): LMDBEither[Unit] = for {
    views <- availableViews()
    _ <- if (v in views) LMDBEither(()) else InvalidView(v).left
  } yield ()

  /**
    * Append a view to the available views
    */
  // todo: check clock validity

  def insertNewView(v: View): LMDBEither[Unit] = transactionalAppendToSet(key, v)

  // todo: optimise by storing the underlying hashtable of the set in LMDB
}

class ViewsLogicalClock(implicit val instance: LMDBInstance) extends MutableCounter[Long]("viewsClock".key) {
  /**
    * Need to define a starting value (the first that will be defined)
    *
    * @return
    */
  override protected def initialValue: Long = 0

  /**
    * Need to define a successor function
    *
    * @param a - old value
    * @return
    */
  override protected def next(a: Long): Long = a + 1
  override def name: String = "Views clock"

  override def db: Dbi[ByteBuffer] = instance.env.openDbi(name, MDB_CREATE)
}
