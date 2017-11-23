package impl.sql.tables

import core.containers.ConstrainedFuture
import core.error.E
import core.view.View
import impl.sql._

class ViewsTable(instance: SQLInstance) {
  import ViewsTable._





}

object ViewsTable {
  val tableName: SQLTableName = ViewsTableName
  val viewID: SQLColumnName = SQLColumnName.viewId
  val commitID: SQLColumnName = SQLColumnName.commitId

  def usingView(v: View, precomputedViewName: PrecomputedView): String =
    s"""
       |CREATE OR REPLACE VIEW $precomputedViewName
       |AS (${getViewIntermediate(v)})
     """.stripMargin

  def wrapView(v: View, precomputedViewName: PrecomputedView)(query: String): String =
    s"""
       |${usingView(v, precomputedViewName)};
       |$query
       |${removeView(precomputedViewName)}
     """.stripMargin

  private def getViewIntermediate(v: View) =
    s"SELECT ${SQLColumnName.commitId} FROM $tableName " +
      s"WHERE ${SQLColumnName.viewId} = ${v.id}"

  def removeView(view: PrecomputedView): String = {
    s"DROP VIEW $view"
  }

  def removeTempViewOp(temporaryViewName: SQLTableName): ConstrainedFuture[E, Unit] = ???
}