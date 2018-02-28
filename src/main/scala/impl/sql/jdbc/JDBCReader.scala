package impl.sql.jdbc

import java.sql.ResultSet

import core.backend.common._
import core.backend.intermediate._
import core.user.containers.Path
import core.user.dsl.ViewId
import core.user.schema._
import core.utils.Logged
import impl.sql._
import impl.sql.adt.queries.PathMemberQuery
import impl.sql.errors.{EmptyResultError, SQLError, SQLExtractError}
import impl.sql.jdbc.Conversions._
import impl.sql.names.{SQLColumnName, SQLTableName, TableNameFromDatabase}
import impl.sql.schema.{ColumnSpecification, SQLType}
import impl.sql.tables.ObjectTable
import impl.sql.types.{Commit, ObjId}

import scalaz.Scalaz._

/**
  * Contains code to read various objects from the results of queries to the SQL db.
  */

class JDBCReader(implicit instance: SQLInstance) extends Logged {

  /**
    * get a single ObjectId
    */

  def getObj(query: String): SQLEither[ObjId] = {
    val rs = getResultSet(query)
    if (rs.next()) {
      getObjId(rs, Single)
    } else EmptyResultError(query).left
  }

  /**
    * Get object ids from result of a query
    * @param query - the query to run
    * @return
    */

  def getObjIds(query: String): SQLEither[Set[ObjId]] = {
    val rs = getResultSet(query)
    var result = Set.newBuilder[ObjId].right[SQLError]
    while (result.isRight && rs.next()) {
      result = for {
        r <- getObjId(rs, Single)
        rs <- result
      } yield rs += r
    }
    result.map(_.result())
  }

  /**
    * Gets a set of SQLTableNames that are defined
    */

  def getTableNames(query: String): SQLEither[Set[SQLTableName]] = {
    val rs = getResultSet(query)
    var result = Set.newBuilder[SQLTableName].right[SQLError]
    while (result.isRight && rs.next()) {
      result = for {
        r <- getTableName(rs)
        rs <- result
      } yield rs += r
    }
    result.map(_.result())
  }

  /**
    * Gets a set of constraint names from the database
    */

  def getConstraint(query: String): SQLEither[Set[(SQLTableName, String)]] = {
    val rs = getResultSet(query)
    var result = Set.newBuilder[(SQLTableName, String)].right[SQLError]
    while (result.isRight && rs.next()) {
      result = for {
        r <- getConstraint(rs, "constraint_name")
        rs <- result
      } yield rs += r
    }
    result.map(_.result())
  }

  /**
    * Get the column specifications for a table in the database
    */

  def getColumns(query: String): SQLEither[List[ColumnSpecification]] = {
    val rs = getResultSet(query)
    var result = List.newBuilder[ColumnSpecification].right[SQLError]
    while (result.isRight && rs.next()) {
      result = for {
        r <- getSQLColumn(rs)
        rs <- result
      } yield rs += r
    }
    result.map(_.result())
  }

  /**
    * Get commitIDs from result of executing query
    * @param query - string of query to execute
    * @return
    */
  def getCommit(query: String): SQLEither[Set[Commit]] = {
    val rs = getResultSet(query)
    var result = Set.newBuilder[Commit].right[SQLError]
    while (result.isRight && rs.next()) {
      result = for {
        r <- getCommitId(rs)
        rs <- result
      } yield rs += r
    }
    result.map(_.result())
  }

  /**
    * Get views from the result of executing a query
    * @param query - query to run
    */

  def getView(query: String): SQLEither[Set[ViewId]] = {
    val rs = getResultSet(query)
    var result = Set.newBuilder[ViewId].right[SQLError]
    while (result.isRight && rs.next()) {
      result = for {
        r <- getViewId(rs)
        rs <- result
      } yield rs += r
    }
    result.map(_.result())
  }


  /**
    * Get a set of the distinct objects in the result of executing a query
    */

  def getDistinctPairs[A, B](query: String)(
    implicit sa: SchemaObject[A], sb: SchemaObject[B]): SQLEither[Set[(A, B)]] = {

    val rs = getResultSet(query)
    val aComponents = sa.schemaComponents
    val bComponents = sb.schemaComponents

    var result = Set.newBuilder[(A, B)].right[SQLError]
    while (result.isRight && rs.next()) {
      var aRow = Vector.newBuilder[DBCell].right[SQLError]
      var bRow = Vector.newBuilder[DBCell].right[SQLError]

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
        a <- sa.fromRow(DBObject(aRes.result())).leftMap(SQLExtractError)
        b <- sb.fromRow(DBObject(bRes.result())).leftMap(SQLExtractError)
        r <- result
      } yield r += pair(a, b)

    }

    result.map(_.result())
  }

  /**
    * Get all distinct singles from executing a query
    */

  def getSingleDistinct[A](query: String)(
    implicit sa: SchemaObject[A]): SQLEither[Set[A]] = {
    val rs = getResultSet(query)
    val aComponents = sa.schemaComponents

    var result = Set.newBuilder[A].right[SQLError]
    while (result.isRight && rs.next() ) {
      var aRow = Vector.newBuilder[DBCell].right[SQLError]

      // Extract vectors from the rs
      for ((component, i) <- aComponents.zipWithIndex) {
        aRow = for {
          cell <- getDBCell(rs, component, i, Single)
          row <- aRow
        } yield row += cell
      }


      result = for {
        aRes <- aRow
        a <- sa.fromRow(DBObject(aRes.result())).leftMap(SQLExtractError)
        r <- result
      } yield r += a
    }
    result.map(_.result())
  }


  /**
    * get a set of all pairs of ids that match a query
    */

  def getRelationPairs(query: String): SQLEither[Set[(ObjId, ObjId)]] = {

    val rs = getResultSet(query)

    var result = Set.newBuilder[(ObjId, ObjId)].right[SQLError]
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

  def getPathfindingEnd[A](a: A)(implicit sa: SchemaObject[A]): SQLEither[Option[ObjId]] = {
    val db = sa.getDBObject(a)

    for {
      table <- instance.lookupTable(sa.name)
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


  def getPathfindingFound[A](
                              ids: List[ObjId],
                              table: ObjectTable,
                              v: ViewId
                            )(implicit sa: SchemaObject[A]): SQLEither[Path[A]] = {
    val query = PathMemberQuery(ids.toSet, sa, table)
    val rs = getResultSet(query.render(v))
    val aComponents = sa.schemaComponents

    var result = Map.newBuilder[ObjId, A].right[SQLError]
    while (result.isRight && rs.next()) {
      var aRow = Vector.newBuilder[DBCell].right[SQLError]

      // Extract vectors from the rs
      for ((component, i) <- aComponents.zipWithIndex) {
        aRow = for {
          cell <- getDBCell(rs, component, i, Single)
          row <- aRow
        } yield row += cell
      }

      result = for {
        aRes <- aRow
        a <- sa.fromRow(DBObject(aRes.result())).leftMap(SQLExtractError)
        id <- getObjId(rs, Single)
        r <- result
      } yield r += (id -> a)
    }
      result.map(_.result()).map(lookup => Path.fromList(ids.map(lookup.apply)))
  }

  /**
    * Pick out A dbcell from the rs at a particular column index
    */

  private def getDBCell(rs: ResultSet, component: SchemaComponent, index: Int, side: Side): SQLEither[DBCell] =
    SQLEither {
      component match {
        case IntCell => DBInt(rs.getInt(side.columnName(index).s))
        case StringCell => DBString(rs.getString(side.columnName(index).s))
        case BoolCell => DBBool(rs.getBoolean(side.columnName(index).s))
        case DoubleCell => DBDouble(rs.getDouble(side.columnName(index).s))
      }
    }

  /**
    * Get a Column specification from the result set
    */

  private def getSQLColumn(rs: ResultSet): SQLEither[ColumnSpecification] =
    SafeEither {
      SQLType.checkString(rs.getString("DATA_TYPE"))
        .map(t => ColumnSpecification(SQLColumnName.extractedSQLColumnName(rs.getString("COLUMN_NAME")), t))
    }

  /**
    * Get a ObjId from the result set
    */
  private def getObjId(rs: ResultSet, side: Side): SQLEither[ObjId] =
    SQLEither {
      ObjId(rs.getLong(side.getId.s))
    }

  /**
    * Get a CommitId from the result set
    */
  private def getCommitId(rs: ResultSet): SQLEither[Commit] =
    SQLEither {
      Commit(rs.getLong(SQLColumnName.commitId.s))
    }

  // helper method
  private def pair[A, B](a: A, b: B): (A, B) = (a, b)

  private def getViewId(rs: ResultSet): SQLEither[ViewId] = SQLEither {
    ViewId(rs.getLong(SQLColumnName.viewId.s))
  }

  /**
    * Run a query
    */
  private def getResultSet(q: String): ResultSet = {
    // log non - write queries
    if(!q.contains("WITH insertOrGetTemp")) logger.info("Read query =  " + q)
    instance
      .connection
      .createStatement()
      .executeQuery(q)
  }

  /**
    * Extract a table name from the result set
    */

  private def getTableName(rs: ResultSet): SQLEither[SQLTableName] =
    SQLEither {
      TableNameFromDatabase(rs.getString("table_name"))
    }

  /**
    * Extract a string from the set
    */

  private def getConstraint(rs: ResultSet, columnName: String): SQLEither[(SQLTableName, String)] =
    SQLEither {
      (TableNameFromDatabase(rs.getString("table_name")), rs.getString(columnName))
    }

}



