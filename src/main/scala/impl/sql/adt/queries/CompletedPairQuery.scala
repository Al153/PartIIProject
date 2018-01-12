package impl.sql.adt.queries

import core.user.dsl.View
import core.backend.intermediate.unsafe.{UnsafeFindPair, ErasedFindable}
import impl.sql._
import impl.sql.adt.{Definitions, Query}
import impl.sql.errors.SQLExtractError
import impl.sql.names.SQLColumnName
import impl.sql.tables.ObjectTable


/**
  * Query renders to find and extract full objects from the database
  * @param p - compiled query
  * @param leftTable - left table to extract from
  * @param rightTable - right table to extract from
  * @param instance - associated SQL instance
  */

case class CompletedPairQuery(
                               p: UnsafeFindPair,
                               leftTable: ObjectTable,
                               rightTable: ObjectTable
                             )(implicit instance: SQLInstance) {

  /**
    * Fully render query to a string to execute against the given view
    */
    def render(v: View): SQLEither[String] =
    // render query to string
      for {
        leftPrototype <- instance.schema.lookupTable(p.leftMostTable).leftMap(SQLExtractError)
        rightPrototype <- instance.schema.lookupTable(p.rightMostTable).leftMap(SQLExtractError)
        res <- Definitions.compute(Query.convertPair(p), v) {
          extractMainQuery(
            leftPrototype.any.getUnsafe,
            rightPrototype.any.getUnsafe,
            leftTable,
            rightTable
          )
        }
      } yield res


  /**
    * Joins extraction-table terms to the left_id, right_id of a main query so that when executed, the result contains all teh extractable fields
    * @param leftProto - a findable used to enumerate left columns
    * @param rightProto - findable used to enumerate right columns
    * @param leftTable - left table to extract from
    * @param rightTable - right table to extract from
    * @return
    */
  private def extractMainQuery(
                                leftProto: ErasedFindable,
                                rightProto: ErasedFindable,
                                leftTable: ObjectTable,
                                rightTable: ObjectTable
                      ): String = {
    s"SELECT ${getColumns(leftProto, rightProto)} " +
      s"FROM ${optionalBrackets(getRightJoin(rightTable, getLeftJoin(leftTable)))}"
  }


 /**
   * Joins left table
  */
  private def getLeftJoin(leftTable: ObjectTable): String =
    s"${getLeftTable(leftTable)} JOIN ${SQLDB.mainQuery} " +
      s"ON ${SQLDB.leftmostTable}.${SQLColumnName.objId} = ${SQLDB.mainQuery}.${SQLColumnName.leftId}"

  /**
    * Joins right tab;e
    */
  private def getRightJoin(rightTable: ObjectTable, leftAndMainQuery: String): String =
    s"($leftAndMainQuery) JOIN ${getRightTable(rightTable)} " +
      s"ON ${SQLColumnName.rightId} = ${SQLDB.rightmostTable}.${SQLColumnName.objId}"

  /**
    * Get the left table alias
    */
  private def getLeftTable(table: ObjectTable): String = s"${table.name} AS ${SQLDB.leftmostTable}"
  /**
    * Get the right table alias
    */
  private def getRightTable(table: ObjectTable): String = s"${table.name} AS ${SQLDB.rightmostTable}"

  /**
    * construct a query to rename columns so left and right columns are distinct
    */
  private def getColumns(
                          leftDescription: ErasedFindable,
                          rightDescription: ErasedFindable
                       ): String = {
    val leftPairs =
      leftDescription.pattern.indices
        .map(i => s"${SQLDB.leftmostTable}.${SQLColumnName.column(i)} AS ${SQLColumnName.leftColumn(i)}")
    val rightPairs =
      rightDescription.pattern.indices
        .map(i => s"${SQLDB.rightmostTable}.${SQLColumnName.column(i)} AS ${SQLColumnName.rightColumn(i)}")

    (leftPairs ++ rightPairs).mkString(", ")
  }



}
