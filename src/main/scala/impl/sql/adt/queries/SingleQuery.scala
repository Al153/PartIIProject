package impl.sql.adt.queries

import core.intermediate.unsafe.UnsafeFindable
import impl.sql.{SQLColumnName, SQLDB}

/**
  * Created by Al on 22/12/2017.
  */
trait SingleQuery {
  protected def getColumns(
                  desc: UnsafeFindable
                ): String =
    if (desc.pattern.nonEmpty) desc.pattern.indices
      .map(i => s"${SQLDB.singleTable}.${SQLColumnName.column(i)} AS ${SQLColumnName.column(i)}")
      .mkString(", ")
    else "0 AS _" // no columns

  protected def getColumnsAndObjId(
                                    desc: UnsafeFindable
                                  ): String =
    if (desc.pattern.nonEmpty) desc.pattern.indices
      .map(i => s"${SQLDB.singleTable}.${SQLColumnName.column(i)} AS ${SQLColumnName.column(i)}")
      .mkString(", ") + s", ${SQLColumnName.objId}"
    else SQLColumnName.objId.toString // no columns
}
