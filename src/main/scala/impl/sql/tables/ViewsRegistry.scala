package impl.sql.tables

import core.containers.ConstrainedFuture
import core.error.E
import core.view.View
import impl.sql.{SQLColumnName, ViewsRegistryName}

class ViewsRegistry {
  import ViewsRegistry._

  def getNewViewId: ConstrainedFuture[E, View] = ???

  // read from the views table
  def getViews: ConstrainedFuture[E, Set[View]] = {
    val query =
      s"""
         |SELECT $viewID FROM $tableName
       """.stripMargin
    ???
  }
}

object ViewsRegistry {
  val tableName = ViewsRegistryName
  val viewID: SQLColumnName = SQLColumnName.viewId
}