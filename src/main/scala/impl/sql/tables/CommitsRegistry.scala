package impl.sql.tables

import java.sql.Statement

import impl.sql._
import impl.sql.errors.UnableToCreateCommit
import impl.sql.schema.{SQLInt, SQLPrimaryRef, SQLSchema}
import impl.sql.tables.ViewsTable.commitID
import impl.sql.types.Commit

import scalaz.Scalaz._

class CommitsRegistry(implicit val instance: SQLInstance) extends SQLTable {
  import CommitsRegistry._
  import instance.executionContext

  def getNewcommitId: SQLFuture[Commit] = SQLFutureE {
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

  override def schema: SQLSchema = SQLSchema(
    Map(
      commitID -> SQLPrimaryRef,
      dummyCol -> SQLInt
    )
  )
  override def name: SQLTableName = CommitsRegistry.name
}

object CommitsRegistry {
  val name = CommitsRegistryName
  val commitId: SQLColumnName = SQLColumnName.commitId
  val dummyCol: SQLColumnName = SQLColumnName.dummyColumn
}