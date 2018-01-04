package impl.sql.schema

import impl.sql.names.{SQLColumnName, SQLTableName}

/**
  * Created by Al on 11/12/2017.
  */
case class SQLSchema(components: Map[SQLColumnName, SQLType], uniqueRelation: Boolean) {
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

    if (uniqueRelation) {
      val uniques = components map {case (name, _) => name} mkString ", "
      s"CREATE TABLE $tableName ($columns, UNIQUE ($uniques))"
    } else {
      s"CREATE TABLE $tableName ($columns)"
    }



  }
}
