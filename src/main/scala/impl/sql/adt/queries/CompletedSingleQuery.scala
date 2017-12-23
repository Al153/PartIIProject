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
                               )(implicit instance: SQLInstance) extends SingleQuery {
  def render(v: View): SQLEither[String] = {
    val (context, q) = Query.convertSingle(p).run(Query.emptyContext)
    for {
      defs <- context.getDefs(instance)
      (tableDefs, relationDefs) = defs

      tablePrototype <- sd.lookupTable(p.table).leftMap(SQLExtractError)

    } yield Definitions.withs(relationDefs, tableDefs, context.commonSubExpressions , v, q) {
      extractMainQuery(tablePrototype.prototype, table)
    }
  }

  private def extractMainQuery(
                                prototype: UnsafeFindable,
                                table: ObjectTable
                              ): String = {
    s"SELECT ${getColumns(prototype)} " +
      s"FROM ${optionalBrackets(getJoin(table))}"
  }

  private def getJoin(table: ObjectTable): String =
    s"${getTable(table)} JOIN ${SQLDB.mainQuery} " +
      s"ON ${SQLDB.singleTable}.${SQLColumnName.objId} = ${SQLDB.mainQuery}.${SQLColumnName.rightId}"

  private def getTable(table: ObjectTable): String = s"${table.name} AS ${SQLDB.singleTable}"
}
