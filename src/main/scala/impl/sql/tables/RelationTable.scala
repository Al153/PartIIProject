package impl.sql.tables

import core.view.View

/**
  * This class represents the DB table matching two
  */

trait RelationTable {

  def getJoin(view: View, previousQuery: QueryString)

}