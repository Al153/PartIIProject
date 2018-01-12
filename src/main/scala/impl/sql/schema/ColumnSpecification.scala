package impl.sql.schema

import impl.sql.names.SQLColumnName

/**
  * Created by Al on 10/12/2017.
  *
  * Schema information that has been extracted from the SQL table itself
  */



case class ColumnSpecification(name: SQLColumnName, colType: String)

