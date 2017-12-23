package impl.sql.adt.queries

import core.intermediate.unsafe.UnsafeFindPair
import core.utils._
import core.view.View
import impl.sql._
import impl.sql.adt.{Definitions, Query}
import impl.sql.tables.ViewsTable

case class PathFindingQuery(p: UnsafeFindPair)(implicit instance: SQLInstance) {

  // should be a query that finds all pairs
  def render(v: View): SQLEither[String] = {
    // render query to string

    val (context, q) = Query.convertPair(p).run(Query.emptyContext)
    for {
      tableDefs <- EitherOps.sequence(
        for {
          (name, sqlName) <- context.getTableDefs
        } yield instance.lookupTable(name).withSnd(sqlName))


      relationDefs <- EitherOps.sequence(for {
        (rel, sqlName) <- context.getRelationDefs
      } yield instance.lookupRelation(rel).withSnd(sqlName))
    } yield Definitions.withs(relationDefs, tableDefs, context.commonSubExpressions , v, q) {
      s"SELECT ${SQLColumnName.leftId}, ${SQLColumnName.rightId} FROM ${optionalBrackets(SQLDB.mainQuery)}"
    }
  }
}
