package impl.sql.errors

case class EmptyResultError(query: String) extends SQLError