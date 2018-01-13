package impl.sql.adt.queries

import core.backend.intermediate.unsafe.{ErasedFindable, UnsafeFindSingle}
import core.user.dsl.View
import impl.sql._
import impl.sql.adt.{Definitions, Query}
import impl.sql.errors.SQLExtractError
import impl.sql.names.SQLColumnName
import impl.sql.tables.ObjectTable

/**
  * Created by Al on 23/11/2017.
  */
case class CompletedSingleQuery(
                                 p: UnsafeFindSingle,
                                 table: ObjectTable
                               )(implicit instance: SQLInstance) extends SingleQuery {
  /**
    * Render the query to an SQL query string that executes against the given view
    * @param v - view to execute against
    * @return
    */
  def render(v: View): SQLEither[String] = {
    for {
      tablePrototype <- instance.schema.lookupTable(p.table).leftMap(SQLExtractError)
      res <- Definitions.compute(Query.convertSingle(p), v) {
        extractMainQuery(tablePrototype.any.getUnsafe, table)
      }
    } yield res
  }

  /**
    * Joins onto the main query's right_id column to get the actual fields out of the DB
    * @param prototype - to generate column indices
    * @param table - table to extract from
    * @return
    */
  private def extractMainQuery(
                                prototype: ErasedFindable,
                                table: ObjectTable
                              ): String = {
    s"SELECT ${getColumns(prototype)} " +
      s"FROM ${optionalBrackets(getJoin(table))}"
  }

  /**
    * Create the joins
    */
  private def getJoin(table: ObjectTable): String =
    s"${getTable(table)} JOIN ${SQLDB.mainQuery} ON ${SQLDB.singleTable}.${SQLColumnName.objId} = ${SQLDB.mainQuery}.${SQLColumnName.rightId}"

  /**
    * Get the tale nae as an alias
    */
  private def getTable(table: ObjectTable): String = s"${table.name} AS ${SQLDB.singleTable}"
}
