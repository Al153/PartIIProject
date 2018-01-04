package impl.sql.jdbc

import core.backend.common._
import impl.sql.names.SQLColumnName
import org.apache.commons.lang.StringEscapeUtils


object Conversions {
  def getColumnNames(db: DBObject): Vector[SQLColumnName] =
    for {
      (_, i) <- db.fields.zipWithIndex
    } yield SQLColumnName.column(i)

  def createComparisons(db: DBObject): String =
    db.fields.zipWithIndex.map {
      case (d, i) =>
        s"${SQLColumnName.column(i)} = ${dbCellToSQLValue(d)}"
    }.mkString(" AND ")


  def dbCellToSQLValue(db: DBCell): String = db match {
    case DBDouble(d) => d.toString
    case DBBool(b) => b.toString
    case DBInt(i) => i.toString
    case DBString(s) => "'" + safeRender(s) + "'"
  }

  def getValues(dBObject: DBObject): Vector[String] = dBObject.fields.map(dbCellToSQLValue)

  def safeRender(s: String): String = StringEscapeUtils.escapeSql(s)
}