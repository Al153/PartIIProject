package impl.sql.tables

import java.sql.Statement

import core.view.View
import impl.sql._
import impl.sql.errors.{SQLError, UnableToCreateView}
import impl.sql.schema.{SQLInt, SQLPrimaryRef, SQLSchema}

import scalaz.Scalaz._

class ViewsRegistry(implicit val instance: SQLInstance) extends SQLTable {
  import ViewsRegistry._
  import instance.executionContext

  def getNewViewId: SQLFuture[View] = SQLFutureE {
    val stmt = instance.connection.prepareStatement(s"INSERT INTO $name($dummyCol) VALUES(0)", Statement.RETURN_GENERATED_KEYS)
    val affectedRows = stmt.executeUpdate()
    if (affectedRows == 0) {
      UnableToCreateView("No rows updated").left
    } else {
      val generatedKeys = stmt.getGeneratedKeys
      if (generatedKeys.next()){
        View(generatedKeys.getLong(viewID.toString)).right[SQLError]
      } else {
        UnableToCreateView("No ID obtained").left
      }
    }
  }


  // read from the views table
  def getViews: SQLFuture[Set[View]] = SQLFutureE {
    instance.reader.getView(
      s"""
         |SELECT $viewID FROM $tableName""".stripMargin
    )
  }

  override def schema: SQLSchema = SQLSchema(Map(viewID -> SQLPrimaryRef, dummyCol -> SQLInt), uniqueRelation = false)

  override def name: SQLTableName = tableName

  /**
    * Need to insert the default view at setup
    * @return
    */

  override protected def create: SQLEither[Unit] = {
    for {
      _ <- instance.doWriteEither(this.schema.create(name))
      _ <- initialiseDefaultView // default view = 0
    } yield ()
  }

  private def initialiseDefaultView: SQLEither[Unit] = {
    instance.doWriteEither(s"INSERT INTO $name ($viewID, $dummyCol) VALUES(0, 0);")
  }
}

object ViewsRegistry {
  val tableName = ViewsRegistryName
  val viewID: SQLColumnName = SQLColumnName.viewId
  val dummyCol: SQLColumnName = SQLColumnName.dummyColumn
}