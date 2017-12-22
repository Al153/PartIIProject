package impl.sql.adt.queries

import core.intermediate.unsafe.{UnsafeFindPair, UnsafeFindable}
import core.utils._
import core.view.View
import impl.sql._
import impl.sql.adt.{Definitions, Query, VarName}
import impl.sql.errors.SQLExtractError
import impl.sql.tables.{ObjectTable, ViewsTable}
import scalaz._, Scalaz._

// todo: need to process views
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
    def render(v: View): SQLEither[String] = {
    // render query to string

    val (context, q) = Query.convertPair(p).run(Query.emptyContext)
    val precomputedView = PrecomputedView() // generate a view to get all the commit ids

    for {
      tableDefs <- EitherOps.sequence(
        for {
          (name, sqlName) <- context.getTableDefs
        } yield instance.lookupTable(name).withSnd(sqlName))


      relationDefs <- EitherOps.sequence(for {
        (rel, sqlName) <- context.getRelationDefs
      } yield instance.lookupRelation(rel).withSnd(sqlName))


      leftPrototype <- instance.schema.lookupTable(p.leftMostTable).leftMap(SQLExtractError)
      rightPrototype <- instance.schema.lookupTable(p.rightMostTable).leftMap(SQLExtractError)

    } yield ViewsTable.wrapView(v, precomputedView) {
      s"""
         |WITH ${Definitions.get(relationDefs, tableDefs, context.commonSubExpressions , q, precomputedView)}
         | ${extractMainQuery(leftPrototype.prototype, rightPrototype.prototype, leftTable, rightTable)};
         |""".stripMargin
    }
  }

  // query terms appended to the left and right hand sides of the main query
  // actually pull out values

  private def extractMainQuery(
                        leftProto: UnsafeFindable,
                        rightProto: UnsafeFindable,
                        leftTable: ObjectTable,
                        rightTable: ObjectTable
                      ): String = {
    s"SELECT ${getColumns(leftProto, rightProto)} " +
      s"FROM (${getRightJoin(rightTable, getLeftJoin(leftTable))});"
  }


  // the joins
  private def getLeftJoin(leftTable: ObjectTable): String =
    s"(${getLeftTable(leftTable)}) JOIN ${SQLDB.mainQuery} " +
      s"ON ${SQLDB.leftmostTable}.${SQLColumnName.objId} = ${SQLDB.mainQuery}.${SQLColumnName.leftId}"

  private def getRightJoin(rightTable: ObjectTable, leftAndMainQuery: String): String =
    s"($leftAndMainQuery) JOIN (${getRightTable(rightTable)}) " +
      s"ON ${SQLColumnName.rightId} = ${SQLDB.rightmostTable}.${SQLColumnName.objId}"

  private def getLeftTable(table: ObjectTable): String = s"${table.name} AS ${SQLDB.leftmostTable}"
  private def getRightTable(table: ObjectTable): String = s"${table.name} AS ${SQLDB.rightmostTable}"

  // construct a query to rename columns
  private def getColumns(
                         leftDescription: UnsafeFindable,
                         rightDescription: UnsafeFindable
                       ): String = {
    val leftPairs =
      (1 to leftDescription.pattern.length)
        .map(i => s"${SQLDB.leftmostTable}.${SQLColumnName.column(i)} AS ${SQLColumnName.leftColumn(i)}")
    val rightPairs =
      (1 to rightDescription.pattern.length)
        .map(i => s"${SQLDB.rightmostTable}.${SQLColumnName.column(i)} AS ${SQLColumnName.rightColumn(i)}")

    val res = (leftPairs ++ rightPairs).mkString(", ")
    println("Left description = " + leftDescription.pattern)
    println("Columns = " + res)
    res
  }



}
