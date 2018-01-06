package impl.lmdb.tables.impl

import core.user.dsl.View
import impl.lmdb.access.Key
import impl.lmdb.access.Key._
import impl.lmdb.tables.interfaces.LMDBTable
import impl.lmdb.{LMDBFuture, LMDBInstance}
import impl.lmdb._
import impl.lmdb.access.Storeable.StoreableView

/**
  * Created by Al on 28/12/2017.
  *
  * Holds the default view of the table
  */
class DefaultViewTable(implicit val instance: LMDBInstance) extends LMDBTable {
  import instance._

  override def path: Key =  "db" >> "default"

  setDefault(initialView)

  def getDefault(): LMDBFuture[View] = LMDBFutureE(get[View](path)(StoreableView, instance))
  def setDefault(v: View): LMDBFuture[Unit] = LMDBFutureE(put(path, v)(StoreableView, instance))

}
