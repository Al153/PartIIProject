package impl.sql.tables

import impl.sql.types.{Commit, ObjId}
import impl.sql.{RelationTableName, SQLColumnName, SQLInstance}

class RelationTable(name: RelationTableName, instance: SQLInstance) {
  import RelationTable._

  def insertRelation(leftId: ObjId, rightId: ObjId, commit: Commit): String =
    s"INSERT INTO $name ($leftIdColumn, $commitId, $rightIdColumn) " +
      s"VALUES ('${leftId.id}', '${commit.id}', ${rightId.id});"

}

object RelationTable {
  val leftIdColumn: SQLColumnName = SQLColumnName.leftId
  val rightIdColumn: SQLColumnName = SQLColumnName.rightId
  val commitId: SQLColumnName = SQLColumnName.commitId

}