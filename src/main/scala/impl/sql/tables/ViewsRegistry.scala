package impl.sql.tables

import java.sql.Statement

import core.user.dsl.ViewId
import impl.sql._
import impl.sql.errors.{SQLError, UnableToCreateView}
import impl.sql.names.{SQLColumnName, SQLTableName, ViewsRegistryName}
import impl.sql.schema.{SQLInt, SQLPrimaryRef, SQLSchema}

import scalaz.Scalaz._

/**
  * Registry stores all current valid views
  * @param instance back-reference to parent DBInstance
  *
  * All views columns in other tables are foreign refs into this one
  */
class ViewsRegistry(implicit val instance: SQLInstance) extends SQLTable {
  import ViewsRegistry._
  import instance.executionContext

  /**
    * Transactionally get a new, unique view Id, using the Dummy column to keep SQL syntax correct
    * @return
    */
  def getNewViewId: SQLFuture[ViewId] = SQLFutureE {
    val stmt = instance.connection.prepareStatement(s"INSERT INTO $name($dummyCol) VALUES(0)", Statement.RETURN_GENERATED_KEYS)
    val affectedRows = stmt.executeUpdate()
    if (affectedRows == 0) {
      UnableToCreateView("No rows updated").left
    } else {
      val generatedKeys = stmt.getGeneratedKeys
      if (generatedKeys.next()){
        ViewId(generatedKeys.getLong(viewID.toString)).right[SQLError]
      } else {
        UnableToCreateView("No ID obtained").left
      }
    }
  }


  /**
    * read all views from the views registry
    */
  def getViews: SQLFuture[Set[ViewId]] = SQLFutureE {
    instance.reader.getView(s"SELECT $viewID FROM $tableName")
  }

  /**
    * Schema is a single viewId column and the Dummy column
    */
  override def schema: SQLSchema = SQLSchema(Map(viewID -> SQLPrimaryRef, dummyCol -> SQLInt), uniqueRelation = false)

  /**
    * Generic name
    */
  override def name: SQLTableName = tableName

  /**
    * Need to insert the default view at setup
    */

  override protected def create: SQLEither[Unit] = {
    for {
      _ <- instance.doWriteEither(this.schema.create(name))
      _ <- initialiseDefaultView // default view = 0
    } yield ()
  }

  /**
    * Sets initial view = 0
    */
  private def initialiseDefaultView: SQLEither[Unit] = {
    instance.doWriteEither(s"INSERT INTO $name ($viewID, $dummyCol) VALUES(0, 0);")
  }
}


object ViewsRegistry {
  // import useful values
  val tableName = ViewsRegistryName
  val viewID: SQLColumnName = SQLColumnName.viewId
  val dummyCol: SQLColumnName = SQLColumnName.dummyColumn
}