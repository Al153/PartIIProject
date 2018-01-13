package impl.lmdb.tables.interfaces

import impl.lmdb.{LMDBEither, LMDBFuture, LMDBInstance}
import impl.lmdb.access.Key

/**
  * Created by Al on 28/12/2017.
  *
  * An LMDB table is a namespace inside the flat LMDB structure
  */
trait LMDBTable {
  def path: Key
  implicit val instance: LMDBInstance
  def initialise(): LMDBEither[Unit]
}
