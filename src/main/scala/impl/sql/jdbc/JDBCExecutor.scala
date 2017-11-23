package impl.sql.jdbc

import java.sql.{Connection, ResultSet}

import core.backend.common._
import core.error.E
import core.schema._
import impl.sql.SQLColumnName

import scalaz.Scalaz._
import scalaz._

class JDBCExecutor(con: Connection) {
  private def pair[A, B](a: A, b: B): (A, B) = (a, b)

  def getObjectPairs[A, B](query: String, leftLength: Int, rightLength: Int)(
    implicit sa: SchemaObject[A], sb: SchemaObject[B]): E \/ Vector[(A, B)] = {

    val stmt = con.createStatement()
    val rs = stmt.executeQuery(query)
    val aComponents = sa.getSchemaComponents
    val bComponents = sb.getSchemaComponents

    var result = Vector.newBuilder[(A, B)].right[E]
    while (result.isRight && rs.next() ) {
      var aRow = Vector.newBuilder[DBCell].right[E]
      var bRow = Vector.newBuilder[DBCell].right[E]

      // Extract vectors from the rs
      for ((component, i) <- aComponents.zipWithIndex) {
        aRow = for {
          cell <- getDBCell(rs, component, i, Left)
          row <- aRow
        } yield row += cell
      }

      for ((component, i) <- bComponents.zipWithIndex) {
        bRow = for {
          cell <- getDBCell(rs, component, i, Right)
          row <- bRow
        } yield row += cell
      }

      result = for {
        aRes <- aRow
        bRes <- bRow
        a <- sa.fromRow(DBObject(aRes.result()))
        b <- sb.fromRow(DBObject(bRes.result()))
        r <- result
      } yield r += pair(a, b)

    }

    result.map(_.result())
  }

  // todo: error handling - research errors thrown
  private def getDBCell(rs: ResultSet, component: SchemaComponent, index: Int, side: Side): E \/ DBCell = {
    component match {
      case IntCell => DBInt(rs.getInt(side.columnName(index).s)).right
      case StringCell => DBString(rs.getString(side.columnName(index).s)).right
      case BoolCell => DBBool(rs.getBoolean(side.columnName(index).s)).right
      case DoubleCell => DBDouble(rs.getDouble(side.columnName(index).s)).right
    }
  }
}

sealed trait Side {
  def columnName(i: Int): SQLColumnName
}
case object Left extends Side {
  override def columnName(i: Int): SQLColumnName = SQLColumnName.leftColumn(i)
}

case object Right extends Side {
  override def columnName(i: Int): SQLColumnName = SQLColumnName.rightColumn(i)
}

