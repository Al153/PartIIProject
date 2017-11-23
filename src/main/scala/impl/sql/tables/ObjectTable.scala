package impl.sql.tables

import core.backend.common.DBObject
import core.containers.ConstrainedFuture
import core.error.E
import core.intermediate.unsafe.UnsafeFindable
import core.view.View
import impl.sql.types.{Commit, ObjId}
import impl.sql.{ObjectTableName, SQLColumnName, SQLDB, SQLInstance}

import scalaz.\/

class ObjectTable(
                   name: ObjectTableName,
                   instance: SQLInstance,
                   prototype: UnsafeFindable
                 ) {
  import ObjectTable._

  def getColumnName(i: Int): E \/ SQLColumnName = ???
  // search for an appropriate object in the view, if there isn't, insert one to the new commit. return the ObjId
  def insertOrGetObject(
                         dBObject: DBObject,
                         view: View,
                         newCommit: Commit
                       ): ConstrainedFuture[E, ObjId] = ???

}

object ObjectTable {
  val objId = SQLColumnName.objId
}