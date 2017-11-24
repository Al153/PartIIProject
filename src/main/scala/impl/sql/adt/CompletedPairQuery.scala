package impl.sql.adt

import core.error.E
import core.intermediate.unsafe.{UnsafeFindPair, UnsafeFindable}
import core.schema.SchemaDescription
import core.utils._
import core.view.View
import impl.sql._
import impl.sql.errors.{SQLRelationMissing, SQLTableMissing}
import impl.sql.tables.{ObjectTable, ViewsTable}

import scalaz.\/

case class CompletedPairQuery(
                               p: UnsafeFindPair,
                               leftTable: ObjectTable,
                               rightTable: ObjectTable,
                               sd: SchemaDescription
                             )(implicit instance: SQLInstance) {
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

      leftPrototype <- sd.lookupTable(p.leftMostTable)
      rightPrototype <- sd.lookupTable(p.rightMostTable)

    } yield ViewsTable.wrapView(v, precomputedView) {
      s"""
         |WITH ${Definitions.get(relationDefs, tableDefs, baseQuery, precomputedView)}
         | ${extractMainQuery(leftPrototype.prototype, rightPrototype.prototype, leftTable, rightTable)}
        """.stripMargin
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
      s"FROM ${getRightJoin(rightTable, getLeftJoin(leftTable))}"
  }


  // the joins
  private def getLeftJoin(leftTable: ObjectTable): String =
    s"${getLeftTable(leftTable)} JOIN ${SQLDB.mainQuery} " +
      s"ON ${SQLDB.leftmostTable}.${SQLColumnName.objId} = ${SQLDB.mainQuery}.${SQLColumnName.leftId}"

  private def getRightJoin(rightTable: ObjectTable, leftAndMainQuery: String): String =
    s"($leftAndMainQuery) JOIN ${getRightTable(rightTable)}" +
      s"ON ${SQLColumnName.rightId} = ${SQLDB.rightmostTable}.${SQLColumnName.objId}"

  private def getLeftTable(table: ObjectTable): String = s"${table.name} as ${SQLDB.leftmostTable}"
  private def getRightTable(table: ObjectTable): String = s"${table.name} as ${SQLDB.rightmostTable}"

  // construct a query to rename columns
  private def getColumns(
                         leftDescription: UnsafeFindable,
                         rightDescription: UnsafeFindable
                       ): String = {
    val leftPairs =
      (1 until leftDescription.pattern.length)
        .map(i => s"${SQLDB.leftmostTable}.${SQLColumnName.column(i)} as ${SQLColumnName.leftColumn(i)}")
    val rightPairs =
      (1 until rightDescription.pattern.length)
        .map(i => s"${SQLDB.rightmostTable}.${SQLColumnName.column(i)} as ${SQLColumnName.rightColumn(i)}")

    (leftPairs ++ rightPairs).mkString(", ")
  }



}