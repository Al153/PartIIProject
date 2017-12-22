package impl.sql.tables

import core.containers.ConstrainedFuture
import core.error.E
import impl.sql._
import impl.sql.schema.{SQLForeignRef, SQLSchema}
import impl.sql.types.{Commit, ObjId}

class RelationTable(val name: RelationTableName, fromTable: ObjectTable,  toTable: ObjectTable)(implicit val instance: SQLInstance) extends SQLTable {
  import RelationTable._
  import instance.executionContext

  def insertRelation(leftId: ObjId, rightId: ObjId, commit: Commit): String =
    s"INSERT INTO $name ($leftIdColumn, $commitId, $rightIdColumn) " +
      s"VALUES (${leftId.id}, ${commit.id}, ${rightId.id});"

  // Todo: need to pass around view name in case of concurrent accesses
  def getExistingRelations(
                            precomputedView: PrecomputedView
                          ): SQLFuture[Set[(ObjId, ObjId)]] = SQLFutureE {
    val q =
      s"""SELECT ${SQLColumnName.leftId}, ${SQLColumnName.rightId} FROM
         | $name JOIN $precomputedView
         | ON $name.$commitId = $precomputedView.$commitId;""".stripMargin
    instance.reader.getRelationPairs(q)
  }

  override def schema: SQLSchema = SQLSchema(
    Map(
      leftIdColumn -> SQLForeignRef(toTable),
      rightIdColumn -> SQLForeignRef(fromTable),
      commitId -> SQLForeignRef(instance.commitsRegistry)
    )
  )
}

object RelationTable {
  val leftIdColumn: SQLColumnName = SQLColumnName.leftId
  val rightIdColumn: SQLColumnName = SQLColumnName.rightId
  val commitId: SQLColumnName = SQLColumnName.commitId

}