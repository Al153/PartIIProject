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
  */
class AvailableViewsTable(implicit val instance: LMDBInstance) extends LMDBTable {
  override def path: Key = "db" >> "views"

  lmdb.put[Set[View]](path, Set(initialView))

  def availableViews(): LMDBEither[Set[View]] = lmdb.get(path)

  def validateView(v: View): LMDBEither[Unit] = for {
    views <- availableViews()
    _ <- if (v in views) LMDBEither(()) else InvalidView(v).left
  } yield ()

  def insertNewView(v: View): LMDBEither[Unit] = transactionalAppendToSet(path, v)
}
