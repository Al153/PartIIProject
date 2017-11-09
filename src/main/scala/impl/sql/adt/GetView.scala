package impl.sql.adt


sealed trait GetView {
  def getSQL: String // get the SQL string
}
