package impl.sql.view

import core.containers.{ConstrainedFuture, Operation}
import core.error.E
import core.view.View
import impl.sql.{SQLColumnName, SQLTableName, ViewsTableName}
import scalikejdbc._

import scalaz.\/

trait ViewsTable {
  import ViewsTable._

  def setDefaultView(view: View): ConstrainedFuture[E, Unit] = ???

  def getDefaultView: Operation[E, Unit] = ???

  // read from the views table
  def getViews: Set[View] = DB readOnly {
    implicit session =>

      val v = ViewRow.syntax("v")
      ???
   //   select(???).from(ViewRow as v).map(ViewRow(v)).list.apply

  //    sql"select distinct $viewID from $tableName".map(rs => rs.long("view_id")).collection.apply()
  }


}

object ViewsTable {
  val tableName: SQLTableName = ViewsTableName
  val viewID = SQLColumnName("view_ID")
  val commitID = SQLColumnName("commit_ID")
}

case class ViewRow(view: View, commit: Commit) {

}

object ViewRow extends SQLSyntaxSupport[ViewRow] {
  override val tableName: String = ViewsTable.tableName.name

}

case class Commit(id: Long) extends AnyVal