package impl.sql.errors

case class UnableToCreateView(msg: String) extends SQLError {
  override def toString: String = s"Unable to create a View: $msg"
}