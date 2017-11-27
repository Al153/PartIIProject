package impl.sql.tables

import core.containers.ConstrainedFuture
import core.error.E
import core.view.View
import impl.sql.{ViewsTableName, _}
import impl.sql.types.Commit

class ViewsTable(implicit instance: SQLInstance) {
  import ViewsTable._


  def removeTempViewOp(temporaryViewName: PrecomputedView): ConstrainedFuture[E, Unit] =
    instance.doWrite(removeView(temporaryViewName))

  def getCommits(view: View): ConstrainedFuture[E, Set[Commit]] = ConstrainedFuture.either {
    val query =
      s"""
         |SELECT $viewID FROM $tableName WHERE $viewID = ${view.id}
       """.stripMargin

    instance.reader.getCommitId(query)
  } (errors.recoverSQLException)

  def insertNewView(view: View, commits: Set[Commit]): ConstrainedFuture[E, Unit] = {
    instance.writeBatch(
      commits.map(
        commit =>
          s"""
             |INSERT INTO $tableName ($viewID, $commitID) VALUES (${view.id}, ${commit.id})
          """.stripMargin
      )
    )
  }

}

object ViewsTable {
  val tableName: ViewsTableName.type = ViewsTableName
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


}