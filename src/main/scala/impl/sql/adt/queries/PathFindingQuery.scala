package impl.sql.adt.queries

import core.user.dsl.View
import core.backend.intermediate.unsafe.UnsafeFindPair
import impl.sql._
import impl.sql.adt.{Definitions, Query}
import impl.sql.names.SQLColumnName

case class PathFindingQuery(p: UnsafeFindPair)(implicit instance: SQLInstance) {

  /**
    * Query that finds related pairs of ObjectIds
    * @param v - view to execute against
    * @return
    */
  def render(v: View): SQLEither[String] = {
    // render query to string
    Definitions.compute(Query.convertPair(p), v) {
      s"SELECT ${SQLColumnName.leftId}, ${SQLColumnName.rightId} FROM ${optionalBrackets(SQLDB.mainQuery)}"
    }
  }
}
