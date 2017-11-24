package impl.sql.errors

case class UnableToCreateCommit(msg: String) extends SQLError {
  override def toString: String = s"Unable to create a commit: $msg"
}