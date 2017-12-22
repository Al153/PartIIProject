package impl.sql.schema

import impl.sql.{SQLColumnName, SQLTableName}

/**
  * Created by Al on 11/12/2017.
  */
case class SQLSchema(components: Map[SQLColumnName, SQLType]) {
  /**
    * returns a query string that creates the table
    * @return
    */
  def create(tableName: SQLTableName): String = {
    val columns =
      components.map {
        case (name, sqlType) =>
          s"${name.s} ${SQLType.toTypeString(sqlType)}"
     }.mkString(",\n")

    s"CREATE TABLE $tableName ($columns);"

  }
}
