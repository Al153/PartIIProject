package impl.sql.names

/**
  * Column name class, for type safety.
  *
  * Doesn't use a type hierarchy, because that would make comparisons between extracted Columns and
  * expected columns harder
  */
class SQLColumnName private (val s: String) extends AnyVal {
  override def toString: String = s
}

object SQLColumnName {
  /**
    * Names for common columns
    */
  val objId = new SQLColumnName("obj_id")
  val leftId = new SQLColumnName("left_id")
  val rightId = new SQLColumnName("right_id")
  val commitId = new SQLColumnName("CommitId")
  val viewId = new SQLColumnName("ViewId")
  val dummyColumn = new SQLColumnName("DUMMY")


  /**
    * Names for columns that are part of objects (eg from schema)
    */
  def column(i: Int): SQLColumnName = new SQLColumnName("col_" + i)
  def leftColumn(i: Int): SQLColumnName = new SQLColumnName("left_col_" + i)
  def rightColumn(i: Int): SQLColumnName = new SQLColumnName("right_col_" + i)

  /**
    * Column names that have been extracted from the database
    */
  def extractedSQLColumnName(n: String) = new SQLColumnName(n)
}