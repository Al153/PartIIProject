package impl.sql.jdbc

import core.backend.common._
import impl.sql.names.SQLColumnName
import org.apache.commons.lang.StringEscapeUtils


/**
  * Conversions of various types into SQL types
  */
object Conversions {
  /**
    * Get the column names of an object
    */
  def getColumnNames(db: DBObject): Vector[SQLColumnName] =
    for {
      (_, i) <- db.fields.zipWithIndex
    } yield SQLColumnName.column(i)

  /**
    * get the string of a SQL query fragment for selecting objects equal to the DBObject
    * in the SQL Database
    * @param db - object to filter for
    * @return
    */

  def createComparisons(db: DBObject): String =
    db.fields.zipWithIndex.map {
      case (d, i) =>
        s"${SQLColumnName.column(i)} = ${dbCellToSQLValue(d)}"
    }.mkString(" AND ")


  /**
    * Safely change a DBCell into a SQL fragment representing the value
    */
  def dbCellToSQLValue(db: DBCell): String = db match {
    case DBDouble(d) => d.toString
    case DBBool(b) => b.toString
    case DBInt(i) => i.toString
    case DBString(s) => "'" + safeRender(s) + "'"
  }

  /**
    * Convert a DBObject into its equivalent SQL values
    */
  def getValues(dBObject: DBObject): Vector[String] = dBObject.fields.map(dbCellToSQLValue)

  /**
    * Safely render a string to be used in SQL (escape apostrophes etc)
    */
  def safeRender(s: String): String = StringEscapeUtils.escapeSql(s)
}