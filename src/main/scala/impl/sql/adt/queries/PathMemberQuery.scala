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
  /**
    * Render a query which picks objects
    *
    * @param v
    * @return
    */
  def render(v: View): String = extractMainQuery(sa.prototype, table)


  private def extractMainQuery(
                                prototype: UnsafeFindable,
                                table: ObjectTable
                              ): String = {
    s"SELECT ${getColumnsAndObjId(prototype)} " +
      s"FROM $table AS ${SQLDB.singleTable} " +
      s"WHERE $conditions"
  }

  private def conditions: String = ids.map(id => s"(${SQLDB.singleTable}.${SQLColumnName.objId} = $id)").mkString(" OR ")
}
