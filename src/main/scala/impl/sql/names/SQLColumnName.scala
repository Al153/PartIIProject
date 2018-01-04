package impl.sql.names

class SQLColumnName private (val s: String) extends AnyVal {
  override def toString: String = s
}

object SQLColumnName {
  val objId = new SQLColumnName("obj_id")
  val leftId = new SQLColumnName("left_id")
  val rightId = new SQLColumnName("right_id")
  val commitId = new SQLColumnName("CommitId")
  val viewId = new SQLColumnName("ViewId")
  val dummyColumn = new SQLColumnName("DUMMY")





  def column(i: Int): SQLColumnName = new SQLColumnName("col_" + i)
  def leftColumn(i: Int): SQLColumnName = new SQLColumnName("left_col_" + i)
  def rightColumn(i: Int): SQLColumnName = new SQLColumnName("right_col_" + i)

  def extractedSQLColumnName(n: String) = new SQLColumnName(n)
}