package impl.sql.tables

import impl.sql.names.{AuxialliaryName, SQLColumnName, SQLTableName}
import impl.sql.schema.{SQLForeignRef, SQLSchema}
import impl.sql.tables.ViewsTable._
import impl.sql.types.{Commit, ObjId}
import impl.sql.{SQLEither, SQLInstance}

/**
  * Created by Al on 27/12/2017.
  *
  * A table which keeps track of which commits an object appears in.
  * Used to select the "identity" relation
  */
class AuxObjectTable(owner: ObjectTable) extends SQLTable {
  import AuxObjectTable._

  /**
    * The schema simply maps objects in its owner to valid commits
    */
  override def schema: SQLSchema = SQLSchema(
      Map(
        objectId -> SQLForeignRef(owner),
        commitId -> SQLForeignRef(instance.commitsRegistry)
      ), uniqueRelation = true
    )

  /**
    * Build a name from the owner's name
    * @return
    */
  override def name: SQLTableName = AuxialliaryName(owner.name)

  /**
    * Inherits instance from owner
    * @return
    */
  override def instance: SQLInstance = owner.instance

  /**
    * Add a relation between a set of ObjIds and a commit
    */
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

  /**
    * Query to get all values associated with the view being read from
    * @return
    */
  def query: String = s"SELECT $objectId AS $leftId, $objectId AS $rightId FROM $name JOIN $viewVar ON $name.$commitId = $viewVar.$commitId"

}

object AuxObjectTable {
  // import a number of useful names
  private val objectId = SQLColumnName.objId
  private val commitId = SQLColumnName.commitId
  private val leftId = SQLColumnName.leftId
  private val rightId = SQLColumnName.rightId
}
