package impl.sql.jdbc

import impl.sql.SQLColumnName

sealed trait Side {
  def columnName(i: Int): SQLColumnName
  def getId: SQLColumnName
}
case object Left extends Side {
  override def columnName(i: Int): SQLColumnName = SQLColumnName.leftColumn(i)
  override def getId: SQLColumnName = SQLColumnName.leftId
}

case object Right extends Side {
  override def columnName(i: Int): SQLColumnName = SQLColumnName.rightColumn(i)
  override def getId: SQLColumnName = SQLColumnName.rightId
}

case object Single extends Side {
  override def columnName(i: Int): SQLColumnName = SQLColumnName.column(i)
  override def getId: SQLColumnName = SQLColumnName.objId
}