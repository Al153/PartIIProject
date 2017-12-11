package impl.sql.schema

import impl.sql.SQLColumnName

/**
  * Created by Al on 10/12/2017.

  */



sealed trait ColumnSpecification {
  def getName: SQLColumnName
  def getType: SQLType
}

