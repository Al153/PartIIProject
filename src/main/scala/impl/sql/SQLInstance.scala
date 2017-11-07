package impl.sql

import core.backend.interfaces.{DBExecutor, DBInstance}
import core.error.E
import core.schema.TableName
import core.view.View
import impl.sql.tables.ObjectTable
import impl.sql.view.ViewsTable

import scalaz.\/
import scalikejdbc._

/**
  * An instance should hold connection pool settings
  * Represents a connection to a database
  */

class SQLInstance extends DBInstance {
  override val executor: DBExecutor = new SQLExecutor(this)
  implicit val session: DBSession = ???

  override def setDefaultView(view: View): \/[E, Unit] = ???

  override def getDefaultView: \/[E, View] = ???

  // read from the views table
  override def getViews: Set[View] = DB readOnly {
    implicit session => sql"select view_id from VIEWS".map(rs => rs.long("view_id")).collection.apply()
  }



  private val viewsTable: ViewsTable = ???
  private val tables: Map[TableName, ObjectTable] = ???

}