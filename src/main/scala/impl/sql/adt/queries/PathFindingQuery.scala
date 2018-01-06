package impl.sql.adt.queries

import core.user.dsl.View
import core.backend.intermediate.unsafe.UnsafeFindPair
import impl.sql._
import impl.sql.adt.{Definitions, Query}
import impl.sql.names.SQLColumnName

case class PathFindingQuery(p: UnsafeFindPair)(implicit instance: SQLInstance) {

  // should be a query that finds all pairs
  def render(v: View): SQLEither[String] = {
    // render query to string
    Definitions.compute(Query.convertPair(p), v) {
      s"SELECT ${SQLColumnName.leftId}, ${SQLColumnName.rightId} FROM ${optionalBrackets(SQLDB.mainQuery)}"
    }
  }
}
