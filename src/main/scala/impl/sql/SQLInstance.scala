package impl.sql

import java.sql.{Connection, SQLException}

import core.backend.common.MissingTableName
import core.backend.interfaces.{DBExecutor, DBInstance}
import core.containers.{ConstrainedFuture, Operation}
import core.error.E
import core.intermediate.unsafe.ErasedRelationAttributes
import core.schema.{SchemaDescription, TableName}
import core.view.View
import impl.sql.errors.{CaughtSQLException, SQLRelationMissing, UnknownSQLException}
import core.utils._
import impl.sql.jdbc.{JDBCReader, JDBCWriter}
import impl.sql.tables._

import scala.concurrent.ExecutionContext
import scalaz.\/

/**
  * An instance should hold connection pool settings
  * Represents a connection to a database
  */

class SQLInstance(
                   val schema: SchemaDescription,
                   instanceID: Long
                 )(implicit val executionContext: ExecutionContext) extends DBInstance {

  override val executor: DBExecutor = new SQLExecutor(this)
  val connection: Connection = ???

  override def setDefaultView(view: View): \/[E, Unit] = ???

  override def getDefaultView: \/[E, View] = ???


  val instanceId: Long = ???

  val viewsTable: ViewsTable = ???
  val viewsRegistry: ViewsRegistry = ???
  val commitsRegistry: CommitsRegistry = ???

  val relationTables: Map[RelationTableName, RelationTable] = ???
  val objectTables: Map[ObjectTableName, ObjectTable] = ???

  // sanitised tableNames
  val tableLookup: Map[TableName, ObjectTable] = ???

  def lookupTable(t: TableName): E \/ ObjectTable = tableLookup.getOrError(t, MissingTableName(t))

  // sanitised relationNames
  val relationLookup: Map[ErasedRelationAttributes, RelationTable] = ???

  def lookupRelation(er: ErasedRelationAttributes): E \/ RelationTable =
    relationLookup.getOrError(er, SQLRelationMissing(er))


  // read from the views table
  override def getViews: ConstrainedFuture[E, Set[View]] = viewsRegistry.getViews

  val reader = new JDBCReader()(this)
  val writer = new JDBCWriter()(this)

  def doWrite(query: String): E ConstrainedFuture Unit = {
    val stmt = connection.createStatement()
    ConstrainedFuture.point[E, Unit] {
      stmt.executeUpdate(query)
      ()
    } (errors.recoverSQLException)
  }

  def writeBatch(queries: TraversableOnce[String]): E ConstrainedFuture Unit = {
    val stmt = connection.createStatement()
    ConstrainedFuture.point[E, Unit] {
      for (query <- queries) stmt.addBatch(query)
      stmt.executeBatch()
      ()
    } (errors.recoverSQLException)
  }
}