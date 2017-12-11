package impl.sql.schema

import impl.sql.SQLColumnName

/**
  * Created by Al on 11/12/2017.
  */
case class SQLSchema(components: Map[SQLColumnName, SQLType])
