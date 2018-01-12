package impl.sql.tables

import java.sql.Statement

import impl.sql._
import impl.sql.errors.UnableToCreateCommit
import impl.sql.names.{CommitsRegistryName, SQLColumnName, SQLTableName}
import impl.sql.schema.{SQLInt, SQLPrimaryRef, SQLSchema}
import impl.sql.tables.ViewsTable.commitID
import impl.sql.types.Commit

import scalaz.Scalaz._

/**
  * The commits registry keeps track of all valid commits
  * @param instance - backreference
  */
class CommitsRegistry(implicit val instance: SQLInstance) extends SQLTable {
  import CommitsRegistry._
  import instance.executionContext

  /**
    * Transactional get-new-commit
    * Creates a new commit and returns the result
    * @return
    */
  def getNewcommitId: SQLFuture[Commit] = SQLFutureE {
    // dummy column used to keep syntax correct
    val stmt = instance.connection.prepareStatement(s"INSERT INTO $name($dummyCol) VALUES (0)", Statement.RETURN_GENERATED_KEYS)

    val affectedRows = stmt.executeUpdate()
    if (affectedRows == 0) {
      UnableToCreateCommit("No rows updated").left
    } else {
      val generatedKeys = stmt.getGeneratedKeys
      if (generatedKeys.next()){
        Commit(generatedKeys.getLong(CommitsRegistry.commitId.toString)).right
      } else {
        UnableToCreateCommit("No ID obtained").left
      }
    }
  }

  /**
    * Contains a commitId which updates serially, and a dummy column, so that we make non-empty writes when getting a new
    * CommitId
    * @return
    */
  override def schema: SQLSchema = SQLSchema(
    Map(
      commitID -> SQLPrimaryRef,
      dummyCol -> SQLInt
    ), uniqueRelation = false
  )

  /**
    * Name is predefined
    * @return
    */
  override def name: SQLTableName = CommitsRegistry.name
}

object CommitsRegistry {
  // import predefined global values
  val name = CommitsRegistryName
  val commitId: SQLColumnName = SQLColumnName.commitId
  val dummyCol: SQLColumnName = SQLColumnName.dummyColumn
}