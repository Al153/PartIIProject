package impl.sql.adt.queries

import core.user.dsl.ViewId
import core.backend.intermediate.unsafe.UnsafeFindPair
import impl.sql._
import impl.sql.adt.{Definitions, Query}
import impl.sql.names.SQLColumnName
import impl.sql.types.ObjId

case class PathFindingQuery(p: UnsafeFindPair)(implicit instance: SQLInstance) {
  import SQLDB._
  import SQLColumnName._

  /**
    * Query that finds related pairs of ObjectIds
    * @param v - view to execute against
    * @return
    */
  def render(v: ViewId): SQLEither[String] = {
    // render query to string
    Definitions.compute(Query.convertPair(p), v) {
      s"SELECT $leftId, $rightId FROM ${optionalBrackets(mainQuery)}"
    }
  }

  def getRight(v: ViewId): SQLEither[ObjId => String] =
    for {
      defs <- Definitions.getDefinitions(Query.convertPair(p), v)
    } yield {
      id: ObjId =>
        defs + s"(SELECT $mainQuery.$rightId AS $objId FROM " +
          s"${optionalBrackets(mainQuery)} WHERE $mainQuery.$leftId = $id)"
    }
}
