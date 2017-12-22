package impl.sql.adt.queries

import core.intermediate.unsafe.{UnsafeFindSingle, UnsafeFindable}
import core.schema.SchemaDescription
import core.utils.{EitherOps, _}
import core.view.View
import impl.sql._
import impl.sql.adt.{Definitions, Query}
import impl.sql.errors.SQLExtractError
import impl.sql.tables.{ObjectTable, ViewsTable}

/**
  * Created by Al on 23/11/2017.
  */
case class CompletedSingleQuery(
                                 p: UnsafeFindSingle,
                                 table: ObjectTable,
                                 sd: SchemaDescription
                               )(implicit instance: SQLInstance) {
  def render(v: View): SQLEither[String] = {
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

      tablePrototype <- sd.lookupTable(p.table).leftMap(SQLExtractError)

    } yield ViewsTable.wrapView(v, precomputedView) {
      s"""
         |WITH ${Definitions.get(relationDefs, tableDefs, context.commonSubExpressions, q, precomputedView)}
         | ${extractMainQuery(tablePrototype.prototype, table)}F""".stripMargin
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

  private def getTable(table: ObjectTable): String = s"${table.name} AS ${SQLDB.singleTable}"

  // construct a query to rename columns
  private def getColumns(
                          desc: UnsafeFindable
                        ): String =
      (1 until desc.pattern.length)
        .map(i => s"${SQLDB.singleTable}.${SQLColumnName.column(i)} AS ${SQLColumnName.leftColumn(i)}")
        .mkString(", ")

}
