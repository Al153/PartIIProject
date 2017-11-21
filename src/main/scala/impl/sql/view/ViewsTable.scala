package impl.sql.view

import core.containers.{ConstrainedFuture, Operation}
import core.error.E
import core.view.View
import impl.sql.{SQLColumnName, SQLDB, SQLTableName, ViewsTableName}
import scalikejdbc._

import scalaz.\/

trait ViewsTable {
  import ViewsTable._

  def setDefaultView(view: View): ConstrainedFuture[E, Unit] = ???

  def getDefaultView: Operation[E, Unit] = ???

  // read from the views table
  def getViews: Set[View] = {
    val query =
      s"""
         |SELECT ${ViewsTable.viewID} FROM ${ViewsTable.tableName}
       """.stripMargin
    ???
  }


}

object ViewsTable {
  // Views table is a relation of ViewId -> commitID
  val tableName: SQLTableName = ViewsTableName
  val viewID: SQLColumnName = SQLColumnName.viewId
  val commitID: SQLColumnName = SQLColumnName.commitId
}

case class ViewRow(view: View, commit: Commit) {

}

object ViewRow extends SQLSyntaxSupport[ViewRow] {
  override val tableName: String = ViewsTable.tableName.name

}

case class Commit(id: Long) extends AnyVal