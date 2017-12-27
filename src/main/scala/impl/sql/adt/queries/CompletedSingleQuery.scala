package impl.sql.adt.queries

import core.intermediate.unsafe.{UnsafeFindSingle, UnsafeFindable}
import core.schema.SchemaDescription
import core.view.View
import impl.sql._
import impl.sql.adt.{Definitions, Query}
import impl.sql.errors.SQLExtractError
import impl.sql.tables.ObjectTable

/**
  * Created by Al on 23/11/2017.
  */
case class CompletedSingleQuery(
                                 p: UnsafeFindSingle,
                                 table: ObjectTable,
                                 sd: SchemaDescription
                               )(implicit instance: SQLInstance) extends SingleQuery {
  def render(v: View): SQLEither[String] = {
    for {
      tablePrototype <- sd.lookupTable(p.table).leftMap(SQLExtractError)
      res <- Definitions.compute(Query.convertSingle(p), v) {
        extractMainQuery(tablePrototype.prototype, table)
      }
    } yield res
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
