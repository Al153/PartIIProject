package impl.sql

class SQLColumnName private (val s: String) extends AnyVal {
  override def toString: String = s
}

object SQLColumnName {
  def apply(s: String): SQLColumnName = new SQLColumnName("USER_" + s)
  val objId = new SQLColumnName("obj_id")
  val leftId = new SQLColumnName("left_id")
  val rightId = new SQLColumnName("right_id")
  val commitId = new SQLColumnName("CommitId")
  val viewId = new SQLColumnName("ViewId")





  def column(i: Int): SQLColumnName = new SQLColumnName("col_" + i)
  def leftColumn(i: Int): SQLColumnName = new SQLColumnName("left_col_" + i)
  def rightColumn(i: Int): SQLColumnName = new SQLColumnName("right_col_" + i)
}