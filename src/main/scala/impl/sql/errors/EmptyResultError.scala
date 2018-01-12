package impl.sql.errors

/**
  * Appears when the result of an SQL query is unexpectedly empty
  */
case class EmptyResultError(query: String) extends SQLError