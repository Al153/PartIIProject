package impl.sql.tables

import java.sql.Statement

import core.containers.ConstrainedFuture
import core.error.E
import impl.sql._
import impl.sql.errors.UnableToCreateCommit
import impl.sql.schema.{SQLForeignRef, SQLPrimaryRef, SQLSchema}
import impl.sql.tables.ViewsTable.{commitID, viewID}
import impl.sql.types.Commit

import scalaz.Scalaz._

class CommitsRegistry(implicit instance: SQLInstance) extends SQLTable {
  import instance.executionContext

  def getNewcommitId: ConstrainedFuture[E, Commit] = ConstrainedFuture.either[E, Commit] {
    val stmt = instance.connection.prepareStatement(s"INSERT INTO $name () VALUES ()", Statement.RETURN_GENERATED_KEYS)

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
  }(errors.recoverSQLException)

  override def schema: SQLSchema = SQLSchema(
    Map(
      commitID -> SQLPrimaryRef
    )
  )
  override def name: SQLTableName = CommitsRegistry.name
}

object CommitsRegistry {
  val name = CommitsRegistryName
  val commitId: SQLColumnName = SQLColumnName.commitId
}