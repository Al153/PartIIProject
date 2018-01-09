package impl.sql.adt.queries

import core.backend.intermediate.unsafe.ErasedFindable
import core.user.dsl.View
import core.user.schema.SchemaObject
import impl.sql._
import impl.sql.names.SQLColumnName
import impl.sql.tables.ObjectTable
import impl.sql.types.ObjId

/**
  * Created by Al on 23/11/2017.
  */
case class PathMemberQuery(
                            ids: TraversableOnce[ObjId],
                            sa: SchemaObject[_],
                            table: ObjectTable
                          )(implicit instance: SQLInstance) extends SingleQuery {
  /**
    * Render a query which picks objects
    *
    * @param v
    * @return
    */
  def render(v: View): String = extractMainQuery(sa.any.getUnsafe, table)


  private def extractMainQuery(
                                prototype: ErasedFindable,
                                table: ObjectTable
                              ): String = {
    s"SELECT ${getColumnsAndObjId(prototype)} " +
      s"FROM $table AS ${SQLDB.singleTable} " +
      s"WHERE $conditions"
  }

  private def conditions: String = ids.map(id => s"(${SQLDB.singleTable}.${SQLColumnName.objId} = $id)").mkString(" OR ")
}
