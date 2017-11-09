package impl.sql

class SQLColumnName private (val s: String) extends AnyVal {
  override def toString: String = s
}

object SQLColumnName {
  def apply(s: String): SQLColumnName = new SQLColumnName("USER_" + s)
  val id = new SQLColumnName("PrimaryId")
  val leftId = new SQLColumnName("LeftId")
  val rightId = new SQLColumnName("RightId")
  val commitId = new SQLColumnName("CommitId")
}