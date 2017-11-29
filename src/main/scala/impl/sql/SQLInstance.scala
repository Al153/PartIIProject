package impl.sql

import java.sql.Connection

import core.backend.common.MissingTableName
import core.backend.interfaces.{DBExecutor, DBInstance}
import core.containers.ConstrainedFuture
import core.error.E
import core.intermediate.unsafe.ErasedRelationAttributes
import core.schema.{SchemaDescription, TableName}
import core.utils._
import core.view.View
import impl.sql.errors.SQLRelationMissing
import impl.sql.jdbc.{JDBCReader, JDBCWriter}
import impl.sql.tables._
import core.utils._
import SQLTableName._

import scala.concurrent.ExecutionContext
import scalaz.\/

/**
  * An instance should hold connection pool settings
  * Represents a connection to a database
  */

class SQLInstance(val connection: Connection, val schema: SchemaDescription, val instanceId: Long)(implicit val executionContext: ExecutionContext) extends DBInstance {

    override val executor: DBExecutor = new SQLExecutor(this)
    val connection: Connection = ???

    val viewsTable: ViewsTable = new ViewsTable()(this)
    val viewsRegistry: ViewsRegistry = new ViewsRegistry()(this)
    val commitsRegistry: CommitsRegistry = new CommitsRegistry()(this)
    val defaultsTable: DefaultsTable = new DefaultsTable()(this)

    val reader = new JDBCReader()(this)
    val writer = new JDBCWriter()(this)



  // sanitised tableNames
  private val tableLookup: Map[TableName, ObjectTable] =
    SQLTableName.render(
      MonadOps.sequence(
        schema.erasedObjects.map {
          o =>
            SQLTableName.getName(o.name) >> (name => o.name -> new ObjectTable(name, this, o))
        }
      )
    ).toMap


  // sanitised relationNames
  private val relationLookup: Map[ErasedRelationAttributes, RelationTable] =
    SQLTableName.render(
      MonadOps.sequence (
        schema.erasedRelations.map {
          er =>
            SQLTableName.getName(er.name) >> (name => er -> new RelationTable(name)(this))
        }
      )
    ).toMap

  override def setDefaultView(view: View): ConstrainedFuture[E, Unit] = defaultsTable.setDefaultView(view)

  override def getDefaultView: ConstrainedFuture[E, View] = defaultsTable.getDefaultView






  /**
    * Safely get the table for a name
    * @param t - the tablename
    * @return
    */
  def lookupTable(t: TableName): E \/ ObjectTable =
    tableLookup.getOrError(t, MissingTableName(t))


  def lookupRelation(er: ErasedRelationAttributes): E \/ RelationTable =
    relationLookup.getOrError(er, SQLRelationMissing(er))


  // read from the views table
  override def getViews: ConstrainedFuture[E, Set[View]] = viewsRegistry.getViews


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