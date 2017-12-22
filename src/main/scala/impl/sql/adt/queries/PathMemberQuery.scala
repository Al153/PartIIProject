package impl.sql.adt.queries

import core.intermediate.unsafe.{SchemaObjectErased, UnsafeFindable}
import core.view.View
import impl.sql._
import impl.sql.adt.Definitions
import impl.sql.tables.{ObjectTable, ViewsTable}
import impl.sql.types.ObjId

/**
  * Created by Al on 23/11/2017.
  */
case class PathMemberQuery(
                            ids: TraversableOnce[ObjId],
                            sa: SchemaObjectErased,
                            table: ObjectTable
                          )(implicit instance: SQLInstance) {
  def render(v: View): String = {
    val precomputedView = PrecomputedView() // generate a view to get all the commit ids
    ViewsTable.wrapView(v, precomputedView) {
      s"""
         |WITH ${SQLDB.singleTable} AS ${Definitions.getTableWithView(table.name, precomputedView)}
         | ${extractMainQuery(sa.prototype, table)}""".stripMargin
    }
  }

  private def extractMainQuery(
                                prototype: UnsafeFindable,
                                table: ObjectTable
                              ): String = {
    s"SELECT ${getColumns(prototype)} " +
      s"FROM ${SQLDB.singleTable} " +
      s"WHERE $conditions"
  }

  // construct a query to rename columns
  private def getColumns(
                          desc: UnsafeFindable
                        ): String =
    (1 until desc.pattern.length)
      .map(i => s"${SQLDB.singleTable}.${SQLColumnName.column(i)} AS ${SQLColumnName.leftColumn(i)}")
      .mkString(", ")

  private def conditions: String = ids.map(id => s"(${SQLDB.singleTable}.${SQLColumnName.objId} = $id)").mkString(" OR ")

}
