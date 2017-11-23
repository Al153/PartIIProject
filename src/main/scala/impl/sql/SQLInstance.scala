package impl.sql

import core.backend.interfaces.{DBExecutor, DBInstance}
import core.containers.ConstrainedFuture
import core.error.E
import core.intermediate.unsafe.ErasedRelationAttributes
import core.schema.{SchemaDescription, TableName}
import core.view.View
import impl.sql.tables._
import scalikejdbc._

import scalaz.\/

/**
  * An instance should hold connection pool settings
  * Represents a connection to a database
  */

class SQLInstance(connectionPool: ConnectionPool, schema: SchemaDescription) extends DBInstance {

  override val executor: DBExecutor = new SQLExecutor(this)

  override def setDefaultView(view: View): \/[E, Unit] = ???

  override def getDefaultView: \/[E, View] = ???




  val viewsTable: ViewsTable = ???
  val viewsRegistry: ViewsRegistry = ???
  val commitRegistry: CommitsRegistry = ???

  val relationTables: Map[RelationTableName, RelationTable] = ???
  val objectTables: Map[ObjectTableName, ObjectTable] = ???

  // sanitised tableNames
  val tableLookup: Map[TableName, ObjectTableName] = ???

  // sanitised relationNames
  val relationLookup: Map[ErasedRelationAttributes, RelationTableName] = ???


  // read from the views table
  override def getViews: ConstrainedFuture[E, Set[View]] = viewsRegistry.getViews
}