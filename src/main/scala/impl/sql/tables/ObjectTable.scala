package impl.sql.tables

import core.backend.common.DBObject
import core.containers.ConstrainedFuture
import core.error.E
import core.intermediate.unsafe.SchemaObjectErased
import core.view.View
import impl.sql.errors.ColumnMismatchException
import impl.sql.types.{Commit, ObjId}
import impl.sql.{ObjectTableName, SQLColumnName, SQLInstance}

import scalaz.Scalaz._
import scalaz._

class ObjectTable(
                   val name: ObjectTableName,
                   instance: SQLInstance,
                   tableSchema: SchemaObjectErased
                 ) {

  def getColumnName(i: Int): E \/ SQLColumnName =
    if (i > 0 &&  i < tableSchema.length)
      SQLColumnName.column(i).right
    else ColumnMismatchException(i, tableSchema.name, tableSchema.schemaComponents).left // todo: Create an error if too large
  
  // search for an appropriate object in the view, if there isn't, insert one to the new commit. return the ObjId
  def insertOrGetObject(
                         dBObject: DBObject,
                         view: View,
                         newCommit: Commit
                       ): ConstrainedFuture[E, ObjId] = ???

}

object ObjectTable {
  val objId: SQLColumnName = SQLColumnName.objId
}