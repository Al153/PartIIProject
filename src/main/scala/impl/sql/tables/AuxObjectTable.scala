package impl.sql.tables

import impl.sql.names.{AuxialliaryName, SQLColumnName, SQLTableName}
import impl.sql.schema.{SQLForeignRef, SQLSchema}
import impl.sql.tables.ViewsTable._
import impl.sql.types.{Commit, ObjId}
import impl.sql.{SQLEither, SQLInstance}

/**
  * Created by Al on 27/12/2017.
  */
class AuxObjectTable(owner: ObjectTable) extends SQLTable {
  import AuxObjectTable._

  override def schema: SQLSchema = SQLSchema(
      Map(
        objectId -> SQLForeignRef(owner),
        commitId -> SQLForeignRef(instance.commitsRegistry)
      ), uniqueRelation = true
    )

  override def name: SQLTableName = AuxialliaryName(owner.name)
  override def instance: SQLInstance = owner.instance

  def insertObjects(ids: Iterable[ObjId], commit: Commit): SQLEither[Unit] = {
    val queries = ids.map(
      id =>

        s"""
         |INSERT INTO $name ($objectId, $commitId) (
         |SELECT $id, ${commit.id}
         |WHERE NOT EXISTS (
         |  SELECT 1 FROM $name WHERE $objectId = $id AND $commitId = ${commit.id}
         |))""".stripMargin
    )
    instance.writeBatchEither(queries)
  }

  def query: String = s"SELECT $objectId AS $leftId, $objectId AS $rightId FROM $name JOIN $viewVar ON $name.$commitId = $viewVar.$commitId"

}

object AuxObjectTable {
  private val objectId = SQLColumnName.objId
  private val commitId = SQLColumnName.commitId
  private val leftId = SQLColumnName.leftId
  private val rightId = SQLColumnName.rightId
}
