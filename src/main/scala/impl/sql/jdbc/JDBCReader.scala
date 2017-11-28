package impl.sql.jdbc

import java.sql.ResultSet

import core.backend.common._
import core.containers.Path
import core.error.E
import core.schema._
import core.view.View
import impl.sql.adt.queries.PathMemberQuery
import impl.sql.errors.EmptyResultError
import impl.sql.tables.ObjectTable
import impl.sql.types.{Commit, ObjId}
import impl.sql.{SQLColumnName, SQLInstance, errors}
import Conversions._

import scalaz.Scalaz._
import scalaz._

/**
  * Contains code to read various objects from the results of queries to the SQL db.
  * @param instance
  */

class JDBCReader(implicit instance: SQLInstance) {

  // get a single ObjectId
  def getObj(query: String): E \/ ObjId = {
    val rs = getResultSet(query)
    if (rs.next()) {
      getObjId(rs, Single)
    } else EmptyResultError(query).left
  }

  /**
    * Get commitIDs from result of executing query
    * @param query - string of query to execute
    * @return
    */
  def getCommit(query: String): E \/  Set[Commit] = {
    val rs = getResultSet(query)
    var result = Set.newBuilder[Commit].right[E]
    while (result.isRight && rs.next()) {
      result = for {
        r <- getCommitId(rs)
        rs <- result
      } yield rs += r
    }

    result.map(_.result())
  }

  /**
    * Get views from the result of executinf a query
    * @param query - query to run
    * @return - E \/ Set[View]
    */

  def getView(query: String): E \/ Set[View] = {
    val rs = getResultSet(query)
    var result = Set.newBuilder[View].right[E]
    while (result.isRight && rs.next()) {
      result = for {
        r <- getViewId(rs)
        rs <- result
      } yield rs += r
    }

    result.map(_.result())
  }

  /**
    * Get all pairs of objects from result of executing a query
    */

  def getAllPairs[A, B](query: String)(
    implicit sa: SchemaObject[A], sb: SchemaObject[B]): E \/ Vector[(A, B)] = {

    val rs = getResultSet(query)
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

  /**
    * Get a set of the distinct objects in the result of executing a query
    */

  def getDistinctPairs[A, B](query: String)(
    implicit sa: SchemaObject[A], sb: SchemaObject[B]): E \/ Set[(A, B)] = {

    val rs = getResultSet(query)
    val aComponents = sa.getSchemaComponents
    val bComponents = sb.getSchemaComponents

    var result = Set.newBuilder[(A, B)].right[E]
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

  /**
    * Get all right hand object from executing a query
    */

  def getAllSingles[A](query: String)(
    implicit sa: SchemaObject[A]): E \/ Vector[A] = {
    val rs = getResultSet(query)
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

  /**
    * Get all distinct singles from executing a query
    */

  def getSingleDistinct[A](query: String)(
    implicit sa: SchemaObject[A]): E \/ Set[A] = {
    val rs = getResultSet(query)
    val aComponents = sa.getSchemaComponents

    var result = Set.newBuilder[A].right[E]
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


  /**
    * get a set of all pairs of ids that match a query
    */

  def getRelationPairs(query: String): E \/ Set[(ObjId, ObjId)] = {

    val rs = getResultSet(query)

    var result = Set.newBuilder[(ObjId, ObjId)].right[E]
    while (result.isRight && rs.next()) {
      result = for {
        a <- getObjId(rs, Left)
        b <- getObjId(rs, Right)
        r <- result
      } yield r += pair(a, b)

    }

    result.map(_.result())
  }

  /**
    * get the id for the end of a pathfinding query, according to a findable
    */

  def getPathfindingEnd[A](a: A)(implicit sa: SchemaObject[A]): E \/ Option[ObjId] = {
    val db = sa.getDBObject(a)

    for {
      table <- instance.lookupTable(sa.tableName)
      pairs = createComparisons(db)
      q = s"""SELECT ${SQLColumnName.objId} FROM $table WHERE $pairs"""
      rs = getResultSet(q)
      res <- if (rs.next()) getObjId(rs, Single).map(Some(_)) else Option.empty[ObjId].right
    } yield res
  }

  /**
    * given a set of IDs, find the objects and return them as a set
    * This could return a map instead?
    */


  def getPathfindingFound[A](ids: TraversableOnce[ObjId], table: ObjectTable, v: View)(implicit sa: SchemaObject[A]): E \/ Path[A] = {
    val query = PathMemberQuery(ids, sa.erased, table)
    val rs = getResultSet(query.render(v))
    val aComponents = sa.getSchemaComponents

    var result = Vector.newBuilder[A].right[E]
    while (result.isRight && rs.next()) {
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
    result.map(_.result()).map(v => Path.from[A](v))
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



  private def getObjId(rs: ResultSet, side: Side) : E \/ ObjId =
    try {
      ObjId(rs.getLong(side.getId.s)).right
    } catch {case e: Throwable => errors.recoverSQLException(e).left}

  private def getCommitId(rs: ResultSet): E \/ Commit =
    try {
      Commit(rs.getLong(SQLColumnName.commitId.s)).right
    } catch {case e: Throwable => errors.recoverSQLException(e).left}

  private def pair[A, B](a: A, b: B): (A, B) = (a, b)

  private def getViewId(rs: ResultSet): E \/ View = try {
    View(rs.getLong(SQLColumnName.viewId.s)).right
  } catch  {case e: Throwable => errors.recoverSQLException(e).left}

  private def getResultSet(q: String): ResultSet = {
    val stmt = instance.connection.createStatement()
    stmt.executeQuery(q)
  }


}



