package impl.sql.adt

/**
  * Compilation context
  */

case class VarName(s: String) extends AnyVal {
  override def toString: String = s
}