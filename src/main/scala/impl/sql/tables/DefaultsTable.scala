package impl.sql.tables

import core.view.View
import impl.sql._
import impl.sql.errors.MissingDefaultViewError
import impl.sql.schema.{SQLForeignRef, SQLSchema}

import scalaz.Scalaz._

class DefaultsTable(implicit val instance: SQLInstance) extends SQLTable {
  import instance.executionContext

  def setDefaultView(v: View): SQLEither[Unit] = {
    println(instance.reader.getView(s"SELECT ${DefaultsTable.viewId} FROM $name"))
    instance.doWriteEither(s"UPDATE $name SET ${DefaultsTable.viewId} = ${v.id}")
  }


  def getDefaultView: SQLFuture[View] = SQLFutureE {
    instance.reader.getView(s"SELECT ${DefaultsTable.viewId} FROM $name").map(_.find(_ => true)).flatMap {
      ov => ov.fold(MissingDefaultViewError.left[View])(_.right)
    }
  }

  /**
    *   Override create in order to set the default view
    */

  override protected def create: SQLEither[Unit] = {
    for {
      _ <- instance.doWriteEither(this.schema.create(name))
      _ <- initialiseDefaultView // default view = 0
    } yield ()
  }

  private def initialiseDefaultView: SQLEither[Unit] = {
    instance.doWriteEither(s"INSERT INTO $name (${DefaultsTable.viewId}) VALUES(0);")
  }

  override def schema: SQLSchema = SQLSchema(
    Map(
      DefaultsTable.viewId -> SQLForeignRef(instance.viewsRegistry)
    )
  )

  override def name: SQLTableName = DefaultsTable.name
}

object DefaultsTable {
  val name = DefaultsTableName
  val viewId: SQLColumnName = SQLColumnName.viewId
}