package impl.sql.tables

import core.view.View
import impl.sql.schema.{SQLForeignRef, SQLSchema}
import impl.sql.types.Commit
import impl.sql.{ViewsTableName, _}

class ViewsTable(implicit val instance: SQLInstance) extends SQLTable {
  import ViewsTable._
  import instance.executionContext


  def removeTempViewOp(temporaryViewName: PrecomputedView): SQLFuture[Unit] =
    instance.doWrite(removeView(temporaryViewName))

  def getCommits(view: View): SQLFuture[Set[Commit]] = SQLFutureE {
    val query =
      s"""
         |SELECT $commitID FROM $tableName WHERE $viewID = ${view.id}""".stripMargin

    instance.reader.getCommit(query)
  }

  def insertNewView(view: View, commits: Set[Commit]):SQLFuture[Unit] =
    instance.writeBatch(
      commits.map(
        commit =>
          s"""
             |INSERT INTO $tableName ($viewID, $commitID) VALUES (${view.id}, ${commit.id})""".stripMargin
      )
    )

  override def schema: SQLSchema = SQLSchema(
    Map(
      viewID -> SQLForeignRef(instance.viewsRegistry),
      commitID -> SQLForeignRef(instance.commitsRegistry)
    ), uniqueRelation = false
  )

  override def name: SQLTableName = tableName
}

object ViewsTable {
  val tableName: ViewsTableName.type = ViewsTableName
  val viewID: SQLColumnName = SQLColumnName.viewId
  val commitID: SQLColumnName = SQLColumnName.commitId

  private[ViewsTable] def definition(v: View): String =
    s"WITH RECURSIVE $viewVar AS (${getViewIntermediate(v)})"

  def withView(v: View)(query: String): String =
    s"${definition(v)} ($query)"
  
  private def getViewIntermediate(v: View) =
    s"SELECT ${SQLColumnName.commitId} FROM $tableName " +
      s"WHERE ${SQLColumnName.viewId} = ${v.id}"

  def removeView(view: PrecomputedView): String = {
    s"DROP VIEW $view"
  }

  implicit class ViewSyntax(v: View) {
    def name: String = viewVar
    def definition: String = ViewsTable.definition(v)
  }

  val viewVar: String = "VIEW_CTE"
}