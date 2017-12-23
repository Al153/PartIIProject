package impl.sql.adt.queries

import core.intermediate.unsafe.{SchemaObjectErased, UnsafeFindable}
import core.view.View
import impl.sql._
import impl.sql.tables.ViewsTable._
import impl.sql.tables.{ObjectTable, ViewsTable}
import impl.sql.types.ObjId

/**
  * Created by Al on 23/11/2017.
  */
case class PathMemberQuery(
                            ids: TraversableOnce[ObjId],
                            sa: SchemaObjectErased,
                            table: ObjectTable
                          )(implicit instance: SQLInstance) extends SingleQuery {
  def render(v: View): String =
    s"""
       | ${v.definition}
       | ${extractMainQuery(sa.prototype, table)}""".stripMargin


  private def extractMainQuery(
                                prototype: UnsafeFindable,
                                table: ObjectTable
                              ): String = {
    s"SELECT ${getColumns(prototype)} " +
      s"FROM $table as ${SQLDB.singleTable} " +
      s"WHERE $conditions"
  }

  private def conditions: String = ids.map(id => s"(${SQLDB.singleTable}.${SQLColumnName.objId} = $id)").mkString(" OR ")
}
