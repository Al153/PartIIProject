package impl.sql.adt.queries

import core.backend.intermediate.unsafe.ErasedFindable
import impl.sql.SQLDB
import impl.sql.names.SQLColumnName

/**
  * Created by Al on 22/12/2017.
  *
  * Common methods for single query
  */
trait SingleQuery {
  /**
    * A query fragment to alias each of the columns
   */
  protected def getColumns(
                  desc: ErasedFindable
                ): String =
    if (desc.pattern.nonEmpty) desc.pattern.indices
      .map(i => s"${SQLDB.singleTable}.${SQLColumnName.column(i)} AS ${SQLColumnName.column(i)}")
      .mkString(", ")
    else "0 AS _" // no columns

  /**
    * A query fragment to alias each of the columns and objectId
    */
  protected def getColumnsAndObjId(
                                    desc: ErasedFindable
                                  ): String =
    if (desc.pattern.nonEmpty) desc.pattern.indices
      .map(i => s"${SQLDB.singleTable}.${SQLColumnName.column(i)} AS ${SQLColumnName.column(i)}")
      .mkString(", ") + s", ${SQLColumnName.objId}"
    else SQLColumnName.objId.toString // no columns
}
