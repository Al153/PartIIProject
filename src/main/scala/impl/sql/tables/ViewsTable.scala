package impl.sql.tables

import core.view.View
import impl.sql.{SQLColumnName, SQLDB, SQLTableName, ViewsTableName}

class ViewsTable {
  import ViewsTable._





}

object ViewsTable {
  val tableName: SQLTableName = ViewsTableName
  val viewID: SQLColumnName = SQLColumnName.viewId
  val commitID: SQLColumnName = SQLColumnName.commitId

  def usingView(v: View)(query: String): String =
    s"""
       |CREATE OR REPLACE VIEW ${SQLDB.temporaryView}
       |AS (${getViewIntermediate(v)})
     """.stripMargin

  private def getViewIntermediate(v: View) =
    s"SELECT ${SQLColumnName.commitId} FROM $tableName " +
      s"WHERE ${SQLColumnName.viewId} = ${v.id}"
}