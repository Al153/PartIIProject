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

import scalaz._
import Scalaz._

/**
  * Created by Al on 28/12/2017.
  *
  * A table containing the views that are currently valid
  */
class AvailableViewsTable(implicit val instance: LMDBInstance) extends LMDBTable {
  // todo: have sub table containing a logical clock, allowing us to store an easily validated cache
  override def path: Key = "db" >> "views"

  // run the initialiser
  lmdb.put[Set[View]](path, Set(initialView))

  /**
    * Get available views in DB
    * @return
    */
  def availableViews(): LMDBEither[Set[View]] = lmdb.get(path)

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
  def insertNewView(v: View): LMDBEither[Unit] = transactionalAppendToSet(path, v)

  // todo: optimise by storing the underlying hashtable of the set in LMDB
}
