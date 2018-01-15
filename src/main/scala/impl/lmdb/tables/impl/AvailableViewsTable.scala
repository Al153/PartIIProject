package impl.lmdb.tables.impl

import core.user.dsl.View
import impl.lmdb.access.Key._
import impl.lmdb.access._
import impl.lmdb.tables.interfaces.LMDBTable
import impl.lmdb.{LMDBEither, LMDBInstance}
import impl.lmdb._
import core.utils._
import impl.lmdb
import impl.lmdb.errors.InvalidView
import org.fusesource.lmdbjni.Database

import scalaz._
import Scalaz._

/**
  * Created by Al on 28/12/2017.
  *
  * A table containing the views that are currently valid
  */
class AvailableViewsTable(implicit val instance: LMDBInstance) extends LMDBTable {
  override val db: Database = instance.env.openDatabase(name)
  private val key = "available".key

  // todo: have sub table containing a logical clock, allowing us to store an easily validated cache
  override def name: String = "db:views"

  /**
    * To initialise, simply set the initial view
    * @return
    */
  override def initialise(): LMDBEither[Unit] = lmdb.put[Set[View]](key, Set(initialView), db)

  /**
    * Get available views in DB
    * @return
    */
  def availableViews(): LMDBEither[Set[View]] = lmdb.get(db, key)

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
  def insertNewView(v: View): LMDBEither[Unit] = transactionalAppendToSet(key, v, db)

  // todo: optimise by storing the underlying hashtable of the set in LMDB
}
