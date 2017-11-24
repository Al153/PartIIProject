package impl.sql.jdbc

import java.sql.ResultSet

import core.backend.common._
import core.error.E
import core.schema._
import impl.sql.{SQLInstance, errors}

import scalaz.Scalaz._
import scalaz._

class JDBCReader(implicit instance: SQLInstance) {
  def getObjectPairs[A, B](query: String)(
    implicit sa: SchemaObject[A], sb: SchemaObject[B]): E \/ Vector[(A, B)] = {

    val stmt = instance.connection.createStatement()
    val rs = stmt.executeQuery(query)
    val aComponents = sa.getSchemaComponents
    val bComponents = sb.getSchemaComponents

    var result = Vector.newBuilder[(A, B)].right[E]
    while (result.isRight && rs.next()) {
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

  def getSingleObject[A](query: String)(
    implicit sa: SchemaObject[A]): E \/ Vector[A] = {
    val stmt = instance.connection.createStatement()
    val rs = stmt.executeQuery(query)
    val aComponents = sa.getSchemaComponents

    var result = Vector.newBuilder[A].right[E]
    while (result.isRight && rs.next() ) {
      var aRow = Vector.newBuilder[DBCell].right[E]

      // Extract vectors from the rs
      for ((component, i) <- aComponents.zipWithIndex) {
        aRow = for {
          cell <- getDBCell(rs, component, i, Single)
          row <- aRow
        } yield row += cell
      }


      result = for {
        aRes <- aRow
        a <- sa.fromRow(DBObject(aRes.result()))
        r <- result
      } yield r += a
    }

    result.map(_.result())
  }

  private def getDBCell(rs: ResultSet, component: SchemaComponent, index: Int, side: Side): E \/ DBCell = {
    try{
      component match {
        case IntCell => DBInt(rs.getInt(side.columnName(index).s)).right
        case StringCell => DBString(rs.getString(side.columnName(index).s)).right
        case BoolCell => DBBool(rs.getBoolean(side.columnName(index).s)).right
        case DoubleCell => DBDouble(rs.getDouble(side.columnName(index).s)).right
      }
    } catch {case e: Throwable => errors.recoverSQLException(e).left}
  }

  private def pair[A, B](a: A, b: B): (A, B) = (a, b)
}



