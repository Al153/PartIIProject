package impl.sql.tables

import java.sql.Statement

import core.containers.ConstrainedFuture
import core.error.E
import impl.sql.errors.UnableToCreateCommit
import impl.sql.types.Commit
import impl.sql.{CommitsRegistryName, SQLColumnName, SQLInstance, errors}

import scala.concurrent.ExecutionContext
import scalaz.Scalaz._

class CommitsRegistry(implicit instance: SQLInstance) {
  import instance.executionContext
  import CommitsRegistry._

  def getNewcommitId: ConstrainedFuture[E, Commit] = ConstrainedFuture.either[E, Commit] {
    val stmt = instance.connection.prepareStatement(s"INSERT INTO $name () VALUES ()", Statement.RETURN_GENERATED_KEYS)

    val affectedRows = stmt.executeUpdate()
    if (affectedRows == 0) {
      UnableToCreateCommit("No rows updated").left
    } else {
      val generatedKeys = stmt.getGeneratedKeys
      if (generatedKeys.next()){
        Commit(generatedKeys.getLong(commitId.toString)).right
      } else {
        UnableToCreateCommit("No ID obtained").left
      }
    }
  }(errors.recoverSQLException)
}

object CommitsRegistry {
  val name = CommitsRegistryName
  val commitId: SQLColumnName = SQLColumnName.commitId
}