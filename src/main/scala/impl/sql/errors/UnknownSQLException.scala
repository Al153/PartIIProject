package impl.sql.errors

case class UnknownSQLException(e: Throwable) extends SQLError {
  override def toString: String = "CAUGHT UNKNOWN SQL EXCEPTION: " + e.toString + "\n" + e.getStackTrace.mkString("\n")
}