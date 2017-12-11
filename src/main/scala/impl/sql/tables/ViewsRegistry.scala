package impl.sql.tables

import java.sql.Statement

import core.containers.ConstrainedFuture
import core.error.E
import core.view.View
import impl.sql.errors.UnableToCreateView
import impl.sql.tables.CommitsRegistry.name
import impl.sql._
import impl.sql.schema.{SQLPrimaryRef, SQLSchema}

import scalaz.Scalaz._
import scalaz._

class ViewsRegistry(implicit instance: SQLInstance) extends SQLTable {
  import ViewsRegistry._
  import instance.executionContext

  def getNewViewId: ConstrainedFuture[E, View] = ConstrainedFuture.either[E, View] {
    val stmt = instance.connection.prepareStatement(s"INSERT INTO $name () VALUES ()", Statement.RETURN_GENERATED_KEYS)
    val affectedRows = stmt.executeUpdate()
    if (affectedRows == 0) {
      UnableToCreateView("No rows updated").left
    } else {
      val generatedKeys = stmt.getGeneratedKeys
      if (generatedKeys.next()){
        View(generatedKeys.getLong(viewID.toString)).right[E]
      } else {
        UnableToCreateView("No ID obtained").left
      }
    }
  }(errors.recoverSQLException)


  // read from the views table
  def getViews: ConstrainedFuture[E, Set[View]] = ConstrainedFuture.either {
    instance.reader.getView(
      s"""
         |SELECT $viewID FROM $tableName
       """.stripMargin
    )
  } (errors.recoverSQLException)

  override def schema: SQLSchema = SQLSchema(Map(viewID -> SQLPrimaryRef))

  override def name: SQLTableName = tableName
}

object ViewsRegistry {
  val tableName = ViewsRegistryName
  val viewID: SQLColumnName = SQLColumnName.viewId
}