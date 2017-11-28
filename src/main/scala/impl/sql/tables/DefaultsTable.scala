package impl.sql.tables

import core.containers.ConstrainedFuture
import core.error.E
import core.view.View
import impl.sql.errors.MissingDefaultViewError
import impl.sql.{DefaultsTableName, SQLColumnName, SQLInstance, errors}

import scalaz._
import Scalaz._

class DefaultsTable(implicit instance: SQLInstance) {
  import DefaultsTable._
  import instance.executionContext

  def setDefaultView(v: View): ConstrainedFuture[E, Unit] =
    instance.doWrite(s"UPDATE $name SET $viewId = ${v.id}")

  def getDefaultView: ConstrainedFuture[E, View] = ConstrainedFuture.either {
    instance.reader.getView(s"SELECT $viewId FROM $name").map(_.find(_ => true)).flatMap {
      ov => ov.fold(MissingDefaultViewError.left[View])(_.right)
    }
  } (errors.recoverSQLException)
}

object DefaultsTable {
  val name = DefaultsTableName
  val viewId: SQLColumnName = SQLColumnName.viewId
}