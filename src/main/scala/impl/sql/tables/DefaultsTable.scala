package impl.sql.tables

import core.user.dsl.ViewId
import impl.sql._
import impl.sql.errors.MissingDefaultViewError
import impl.sql.names.{DefaultsTableName, SQLColumnName, SQLTableName}
import impl.sql.schema.{SQLForeignRef, SQLSchema}

import scalaz.Scalaz._

/**
  * Single-element table that keeps track of the default View
  * @param instance
  */
class DefaultsTable(implicit val instance: SQLInstance) extends SQLTable {
  import instance.executionContext

  /**
    * Set value with simple update
    */
  def setDefaultView(v: ViewId): SQLEither[Unit] = {
    instance.doWriteEither(s"UPDATE $name SET ${DefaultsTable.viewId} = ${v.id}")
  }

  /**
    * Get value with simple SELECT
    */
  def getDefaultView: SQLFuture[ViewId] = SQLFutureE {
    instance.reader.getView(s"SELECT ${DefaultsTable.viewId} FROM $name").map(_.find(_ => true)).flatMap {
      ov => ov.fold(MissingDefaultViewError.left[ViewId])(_.right)
    }
  }

  /**
    *   Override create in order to set the default view when table is newly generated in SQL
    */

  override protected def create: SQLEither[Unit] = {
    for {
      _ <- instance.doWriteEither(this.schema.create(name))
      _ <- initialiseDefaultView // default view = 0
    } yield ()
  }

  /**
    * Set up the default view = 0
    */
  private def initialiseDefaultView: SQLEither[Unit] = {
    instance.doWriteEither(s"INSERT INTO $name (${DefaultsTable.viewId}) VALUES(0)")
  }

  /**
    * Single row, single column
    */
  override def schema: SQLSchema = SQLSchema(
    Map(
      DefaultsTable.viewId -> SQLForeignRef(instance.viewsRegistry)
    ), uniqueRelation = false
  )

  /**
    * Use predefined name
    */
  override def name: SQLTableName = DefaultsTable.name
}

object DefaultsTable {
  // import predefined values
  val name = DefaultsTableName
  val viewId: SQLColumnName = SQLColumnName.viewId
}