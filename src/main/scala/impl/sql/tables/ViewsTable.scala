package impl.sql.tables

import core.user.dsl.ViewId
import impl.sql._
import impl.sql.names.{SQLColumnName, SQLTableName, ViewsTableName}
import impl.sql.schema.{SQLForeignRef, SQLSchema}
import impl.sql.types.Commit

/**
  * Table keeps track of which views contain which commits
  * @param instance - back reference to owning instance
  */
class ViewsTable(implicit val instance: SQLInstance) extends SQLTable {
  import ViewsTable._
  import instance.executionContext

  /**
    * Get commits associated with a view
    */
  def getCommits(view: ViewId): SQLFuture[Set[Commit]] = SQLFutureE {
    val query =
      s"""
         |SELECT $commitID FROM $tableName WHERE $viewID = ${view.id}""".stripMargin

    instance.reader.getCommit(query)
  }

  /**
    * Inserts a new view with the relevant commits
    */
  def insertNewView(view: ViewId, commits: Set[Commit]):SQLFuture[Unit] =
    instance.writeBatch(
      commits.map(
        commit =>
          s"""
             |INSERT INTO $tableName ($viewID, $commitID) VALUES (${view.id}, ${commit.id})""".stripMargin
      )
    )

  /**
    * Schema is pairs of (view, commit)
   */
  override def schema: SQLSchema = SQLSchema(
    Map(
      viewID -> SQLForeignRef(instance.viewsRegistry),
      commitID -> SQLForeignRef(instance.commitsRegistry)
    ), uniqueRelation = false
  )

  /**
    * Generic name
    * @return
    */
  override def name: SQLTableName = tableName
}

object ViewsTable {
  /**
    * Import useful values
    */
  val tableName: ViewsTableName.type = ViewsTableName
  val viewID: SQLColumnName = SQLColumnName.viewId
  val commitID: SQLColumnName = SQLColumnName.commitId

  /**
    * Syntactic sugar for wrapping a query in a view_cte expression
    */

  def withView(v: ViewId)(query: String): String =
    s"${definition(v)} ($query)"


  /**
    * The definition of a view is a subquery which selects all relevant Commits
    * to a view.
    *
    * This sets up a CTE for the commits.
    * This can be joined with, for example, relation tables to limit the results to
    * relations from a given view

    */
  private[ViewsTable] def definition(v: ViewId): String =
    s"WITH RECURSIVE $viewVar AS (SELECT ${SQLColumnName.commitId} FROM $tableName " +
      s"WHERE ${SQLColumnName.viewId} = ${v.id})"

  /**
    * Syntax for views
    */
  implicit class ViewSyntax(v: ViewId) {
    def definition: String = ViewsTable.definition(v)
  }

  /**
    * Generic name the subexpression used as an alias for the view in question
    */
  val viewVar: String = "VIEW_CTE"
}