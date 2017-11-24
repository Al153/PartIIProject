package impl.sql.jdbc

import impl.sql.SQLColumnName

sealed trait Side {
  def columnName(i: Int): SQLColumnName
}
case object Left extends Side {
  override def columnName(i: Int): SQLColumnName = SQLColumnName.leftColumn(i)
}

case object Right extends Side {
  override def columnName(i: Int): SQLColumnName = SQLColumnName.rightColumn(i)
}

case object Single extends Side {
  override def columnName(i: Int): SQLColumnName = SQLColumnName.column(i)
}