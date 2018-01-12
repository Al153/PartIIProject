package impl.sql.adt

/**
  * Value class for variable aliases
  */

case class VarName(s: String) extends AnyVal {
  override def toString: String = s
}