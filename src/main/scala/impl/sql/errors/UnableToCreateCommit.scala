package impl.sql.errors

/**
  * Thrown when creation of a commit fails
  */
case class UnableToCreateCommit(msg: String) extends SQLError {
  override def toString: String = s"Unable to create a commit: $msg"
}