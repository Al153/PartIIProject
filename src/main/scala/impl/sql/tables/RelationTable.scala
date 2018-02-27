package impl.sql.tables

import ViewsTable._
import core.user.dsl.ViewId
import impl.sql._
import impl.sql.names.{RelationTableName, SQLColumnName}
import impl.sql.schema.{SQLForeignRef, SQLSchema}
import impl.sql.types.{Commit, ObjId}

/**
  * Table per relation in instance's SchemaDescription
  *
  * Base table that is joined to get relational queries
  *
  * Columns are (left_id, commit, right_id)
 */
class RelationTable(val name: RelationTableName, leftTable: ObjectTable, rightTable: ObjectTable)(implicit val instance: SQLInstance) extends SQLTable {
  import RelationTable._
  import instance.executionContext

  /**
    * Get a query to insert a relation instance at a commit
    */
  def insertRelation(leftId: ObjId, rightId: ObjId, commit: Commit): String =
    s"INSERT INTO $name ($leftIdColumn, $commitId, $rightIdColumn) " +
      s"VALUES (${leftId.id}, ${commit.id}, ${rightId.id})"

  /**
    * Find all existing relations visible in a view
    */

  def getExistingRelations(
                            view: ViewId
                          ): SQLFuture[Set[(ObjId, ObjId)]] = SQLFutureE {
    val q = withView(view){
      s"""
         |SELECT ${SQLColumnName.leftId}, ${SQLColumnName.rightId} FROM
         | $name JOIN $viewVar
         | ON $name.$commitId = $viewVar.$commitId""".stripMargin

    }
    instance.reader.getRelationPairs(q)
  }

  /**
    * Schema consists of triples (left, commit, right)
    * @return
    */
  override def schema: SQLSchema = SQLSchema(
    Map(
      leftIdColumn -> SQLForeignRef(leftTable),
      rightIdColumn -> SQLForeignRef(rightTable),
      commitId -> SQLForeignRef(instance.commitsRegistry)
    ), uniqueRelation = true
  )
}

object RelationTable {
  // import useful values
  val leftIdColumn: SQLColumnName = SQLColumnName.leftId
  val rightIdColumn: SQLColumnName = SQLColumnName.rightId
  val commitId: SQLColumnName = SQLColumnName.commitId

}