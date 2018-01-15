package impl.lmdb.tables.interfaces

import impl.lmdb.{LMDBEither, LMDBInstance}
import org.fusesource.lmdbjni.Database

/**
  * Created by Al on 28/12/2017.
  *
  * An LMDB table is a namespace inside the flat LMDB structure
  */
trait LMDBTable {
  def name: String
  implicit val instance: LMDBInstance
  def db: Database
  def initialise(): LMDBEither[Unit]

}
