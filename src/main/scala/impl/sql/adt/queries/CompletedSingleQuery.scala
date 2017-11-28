package impl.sql.adt.queries

import core.error.E
import core.intermediate.unsafe.{UnsafeFindSingle, UnsafeFindable}
import core.schema.SchemaDescription
import core.utils.{EitherOps, _}
import core.view.View
import impl.sql._
import impl.sql.adt.{Definitions, Query}
import impl.sql.errors.{SQLRelationMissing, SQLTableMissing}
import impl.sql.tables.{ObjectTable, ViewsTable}

import scalaz.\/

/**
  * Created by Al on 23/11/2017.
  */
case class CompletedSingleQuery(
                                 p: UnsafeFindSingle,
                                 table: ObjectTable,
                                 sd: SchemaDescription
                               )(implicit instance: SQLInstance) {
  def render(v: View): E \/ String = {
    val (context, q) = Query.convertSingle(p).run(Query.emptyContext)
    val precomputedView = PrecomputedView() // generate a view to get all the commit ids

    for {
      tableDefs <- EitherOps.sequence(
        for {
          (name, sqlName) <- context.getTableDefs
        } yield instance.lookupTable(name).withSnd(sqlName))


      relationDefs <- EitherOps.sequence(for {
        (rel, sqlName) <- context.getRelationDefs
      } yield instance.lookupRelation(rel).withSnd(sqlName))


      baseQuery = Query.render(q)

      tablePrototype <- sd.lookupTable(p.table)

    } yield ViewsTable.wrapView(v, precomputedView) {
      s"""
         |WITH ${Definitions.get(relationDefs, tableDefs, baseQuery, precomputedView)}
         | ${extractMainQuery(tablePrototype.prototype, table)}
        """.stripMargin
    }
  }

  private def extractMainQuery(
                                prototype: UnsafeFindable,
                                table: ObjectTable
                              ): String = {
    s"SELECT ${getColumns(prototype)} " +
      s"FROM ${getJoin(table)}"
  }

  private def getJoin(table: ObjectTable): String =
    s"${getTable(table)} JOIN ${SQLDB.mainQuery} " +
      s"ON ${SQLDB.singleTable}.${SQLColumnName.objId} = ${SQLDB.mainQuery}.${SQLColumnName.rightId}"

  private def getTable(table: ObjectTable): String = s"${table.name} as ${SQLDB.singleTable}"

  // construct a query to rename columns
  private def getColumns(
                          desc: UnsafeFindable
                        ): String =
      (1 until desc.pattern.length)
        .map(i => s"${SQLDB.singleTable}.${SQLColumnName.column(i)} as ${SQLColumnName.leftColumn(i)}")
        .mkString(", ")

}
