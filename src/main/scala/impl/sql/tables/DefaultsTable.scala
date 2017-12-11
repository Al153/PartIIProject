package impl.sql.tables

import core.containers.ConstrainedFuture
import core.error.E
import core.view.View
import impl.sql.errors.MissingDefaultViewError
import impl.sql._
import impl.sql.schema.{SQLForeignRef, SQLSchema}

import scalaz._
import Scalaz._

class DefaultsTable(implicit instance: SQLInstance) extends SQLTable {
  import instance.executionContext

  def setDefaultView(v: View): ConstrainedFuture[E, Unit] =
    instance.doWrite(s"UPDATE $name SET ${DefaultsTable.viewId} = ${v.id}")

  def getDefaultView: ConstrainedFuture[E, View] = ConstrainedFuture.either {
    instance.reader.getView(s"SELECT ${DefaultsTable.viewId} FROM $name").map(_.find(_ => true)).flatMap {
      ov => ov.fold(MissingDefaultViewError.left[View])(_.right)
    }
  } (errors.recoverSQLException)

  override def schema: SQLSchema = SQLSchema(
    Map(
      DefaultsTable.viewId -> SQLForeignRef(instance.viewsRegistry)
    )
  )

  override def name: SQLTableName = DefaultsTable.name
}

object DefaultsTable {
  val name = DefaultsTableName
  val viewId: SQLColumnName = SQLColumnName.viewId
}