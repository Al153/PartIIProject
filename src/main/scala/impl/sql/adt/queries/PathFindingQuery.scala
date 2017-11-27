package impl.sql.adt.queries

import core.error.E
import core.intermediate.unsafe.UnsafeFindPair
import core.utils._
import core.view.View
import impl.sql._
import impl.sql.adt.{Definitions, Query}
import impl.sql.errors.{SQLRelationMissing, SQLTableMissing}
import impl.sql.tables.ViewsTable

import scalaz.\/

case class PathFindingQuery(p: UnsafeFindPair)(implicit instance: SQLInstance) {

  // should be a query that finds all pairs
  def render(v: View): E \/ String = {
    // render query to string

    val (context, q) = Query.convertPair(p).run(Query.emptyContext)
    val precomputedView = PrecomputedView() // generate a view to get all the commit ids

    for {
      tableDefs <- EitherOps.sequence(
        for {
          (name, sqlName) <- context.getTableDefs
        } yield instance.tableLookup.getOrError(name, SQLTableMissing(name)).withSnd(sqlName))


      relationDefs <- EitherOps.sequence(for {
        (rel, sqlName) <- context.getRelationDefs
      } yield instance.relationLookup.getOrError(rel, SQLRelationMissing(rel)).withSnd(sqlName))


      baseQuery = Query.render(q)



    } yield ViewsTable.wrapView(v, precomputedView) {
      s"""
         |WITH ${Definitions.get(relationDefs, tableDefs, baseQuery, precomputedView)}
         |SELECT ${SQLColumnName.leftId}, ${SQLColumnName.rightId} FROM ${SQLDB.mainQuery}
        """.stripMargin
    }
  }
}
