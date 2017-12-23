package impl.sql.tables

import ViewsTable._
import core.view.View
import impl.sql._
import impl.sql.schema.{SQLForeignRef, SQLSchema}
import impl.sql.types.{Commit, ObjId}

class RelationTable(val name: RelationTableName, leftTable: ObjectTable, rightTable: ObjectTable)(implicit val instance: SQLInstance) extends SQLTable {
  import RelationTable._
  import instance.executionContext

  def insertRelation(leftId: ObjId, rightId: ObjId, commit: Commit): String =
    s"INSERT INTO $name ($leftIdColumn, $commitId, $rightIdColumn) " +
      s"VALUES (${leftId.id}, ${commit.id}, ${rightId.id})"

  // Todo: need to pass around view name in case of concurrent accesses
  def getExistingRelations(
                            view: View
                          ): SQLFuture[Set[(ObjId, ObjId)]] = SQLFutureE {
    val q = withView(view){
      s"""
         |SELECT ${SQLColumnName.leftId}, ${SQLColumnName.rightId} FROM
         | $name JOIN ${view.name}
         | ON $name.$commitId = ${view.name}.$commitId""".stripMargin

    }
    instance.reader.getRelationPairs(q)
  }

  override def schema: SQLSchema = SQLSchema(
    Map(
      leftIdColumn -> SQLForeignRef(leftTable),
      rightIdColumn -> SQLForeignRef(rightTable),
      commitId -> SQLForeignRef(instance.commitsRegistry)
    )
  )
}

object RelationTable {
  val leftIdColumn: SQLColumnName = SQLColumnName.leftId
  val rightIdColumn: SQLColumnName = SQLColumnName.rightId
  val commitId: SQLColumnName = SQLColumnName.commitId

}