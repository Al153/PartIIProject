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
import impl.sql.errors.{SQLError, SQLExtractError, SQLRelationMissing}
import impl.sql.jdbc.{JDBCReader, JDBCWriter}
import impl.sql.tables._
import core.utils._
import SQLTableName._

import scala.concurrent.ExecutionContext
import scalaz.\/
import scalaz.Scalaz.ToEitherOps

/**
  * An instance should hold connection pool settings
  * Represents a connection to a database
  */

class SQLInstance(val connection: Connection, val schema: SchemaDescription)(implicit val executionContext: ExecutionContext) extends DBInstance {

  override val executor: DBExecutor = new SQLExecutor(this)
  val viewsTable: ViewsTable = new ViewsTable()(this)
  val viewsRegistry: ViewsRegistry = new ViewsRegistry()(this)
  val commitsRegistry: CommitsRegistry = new CommitsRegistry()(this)
  val defaultsTable: DefaultsTable = new DefaultsTable()(this)

  val reader = new JDBCReader()(this)
  val writer = new JDBCWriter()(this)

  private val constructionTables: Seq[(SQLTableName, SQLTable)] =
    List(viewsTable, viewsRegistry, commitsRegistry, defaultsTable).map(t => t.name -> t)



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
  private val relationLookup: E \/ Map[ErasedRelationAttributes, RelationTable] =
    EitherOps.sequence(SQLTableName.render(
      MonadOps.sequence (
        schema.erasedRelations.map {
          er =>
            for {
              name <- SQLTableName.getName(er.name)
            } yield for {
              fromTable <- lookupTable(er.from)
              toTable <- lookupTable(er.to)
            } yield er -> new RelationTable(name, fromTable, toTable)(this)
        }
      )
    )).map(_.toMap)

  override def setDefaultView(view: View): SQLFuture[Unit] = SQLFutureE(defaultsTable.setDefaultView(view))

  override def getDefaultView: SQLFuture[View] = defaultsTable.getDefaultView






  /**
    * Safely get the table for a name
    * @param t - the tablename
    * @return
    */
  def lookupTable(t: TableName): SQLEither[ObjectTable] =
    tableLookup.getOrError(t, MissingTableName(t)).leftMap(SQLExtractError)


  def lookupRelation(er: ErasedRelationAttributes): E \/ RelationTable =
    relationLookup.flatMap(_.getOrError(er, SQLRelationMissing(er)))


  // read from the views table
  override def getViews: ConstrainedFuture[E, Set[View]] = viewsRegistry.getViews


  def doWrite(query: String): SQLFuture[Unit] = SQLFutureE {doWriteEither(query)}


  def doWriteEither(query: String): SQLEither[Unit] = try {
    val fixedQuery = if (query.endsWith(";")) query else query + ";"
    println(fixedQuery)
    val stmt = connection.createStatement()
    stmt.executeUpdate(fixedQuery)
    ().right
  } catch {case e: Throwable => errors.recoverSQLException(e).left}

  // Returns an error if the tables are invalid
  def validateTables(): SQLFuture[Unit] =
    SQLFutureE(
      getDefinedTables().flatMap(
        definedTables =>
          EitherOps.sequence(
            for {
              (name, table) <- tableLookup ++ constructionTables
            } yield table.validateOrCreate(definedTables)) map (_ => ())
      )
    )


  private def getDefinedTables(): SQLEither[Set[SQLTableName]] = {
    val q = s"SELECT table_name FROM information_schema.tables WHERE table_schema='public';"
    reader.getTableNames(q)
  }

  // drop all existing user tables
  def freshen(): SQLFuture[Unit] = SQLFutureE {
    for {
      tables <- getDefinedTables()
      _ <- if (tables.nonEmpty) doWriteEither(s"DROP TABLE ${tables.mkString(", ")};") else ().right
    } yield  ()
  }

  def writeBatch(queries: TraversableOnce[String]): E ConstrainedFuture Unit = {
    val stmt = connection.createStatement()
    ConstrainedFuture.point[E, Unit] {
      for (query <- queries) stmt.addBatch(query)
      stmt.executeBatch()
    } (errors.recoverSQLException)
  }
}