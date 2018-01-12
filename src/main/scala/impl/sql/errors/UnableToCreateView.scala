package impl.sql.errors

/**
  * Thrown when creation of a view fails
  */
case class UnableToCreateView(msg: String) extends SQLError {
  override def toString: String = s"Unable to create a View: $msg"
}