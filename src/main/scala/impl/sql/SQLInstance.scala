package impl.sql

import java.sql.Connection

import core.backend.common.MissingTableName
import core.user.interfaces.{DBExecutor, DBInstance}
import core.user.containers.ConstrainedFuture
import core.user.dsl.{E, View}
import core.backend.intermediate.unsafe.ErasedRelationAttributes
import core.user.schema.{SchemaDescription, TableName}
import core.utils._
import impl.sql.errors.{SQLErasedRelationMissing, SQLError, SQLExtractError}
import impl.sql.jdbc.{JDBCReader, JDBCWriter}
import impl.sql.tables._
import core.utils._
import impl.sql.names.SQLTableName

import scala.concurrent.ExecutionContext
import scalaz.\/
import scalaz._
import Scalaz._

/**
  * An instance should hold connection pool settings
  * Represents a connection to a database
  *
  * implements [[DBInstance]]
  */

class SQLInstance(val connection: Connection, val schema: SchemaDescription)(implicit val executionContext: ExecutionContext) extends DBInstance {
  override val executor: DBExecutor = new SQLExecutor(this)

  /**
    * To close, close the connection
    */

  override def close(): Unit = connection.close()

  // Views table relates each view to the commits it contains
  val viewsTable: ViewsTable = new ViewsTable()(this)

  // views registry contains all valid views
  val viewsRegistry: ViewsRegistry = new ViewsRegistry()(this)

  // commits registry contains all valid commits
  val commitsRegistry: CommitsRegistry = new CommitsRegistry()(this)

  // Defaults table holds the default view
  val defaultsTable: DefaultsTable = new DefaultsTable()(this)

  // set up db readers and writers
  val reader = new JDBCReader()(this)
  val writer = new JDBCWriter()(this)


  // sanitised tableNames
  private val tableLookup: Map[TableName, ObjectTable] =
    schema.objects.foldLeft(SQLTableName.point(Map[TableName, ObjectTable]())) {
      case (cm, o) =>
        for {
          m <- cm
          name <- SQLTableName.getName(o.name)
        } yield m + (o.name -> new ObjectTable(name, this, o))
    } |> SQLTableName.render




  // sanitised relationNames, each is unique
  private val relationLookup: SQLEither[Map[ErasedRelationAttributes, RelationTable]] =
    schema
      .erasedRelations
      .foldLeft(SQLEither(SQLTableName.point(Map[ErasedRelationAttributes, RelationTable]()))) {
      case (ecm, er) =>
        for {
          cm <- ecm
          fromTable <- lookupTable(er.from)
          toTable <- lookupTable(er.to)

          res = for {
            m <- cm
            name <- SQLTableName.getName(er.name)
          } yield m + (er -> new RelationTable(name, fromTable, toTable)(this))
        } yield res
    } map SQLTableName.render



  /**
    * Delegates setDefaultView to the defaults table
    */
  override def setDefaultView(view: View): ConstrainedFuture[E, Unit] = SQLFutureE(defaultsTable.setDefaultView(view)).asCFuture

  /**
    * Delegates getDefaultView to the defaults table
    */
  override def getDefaultView: ConstrainedFuture[E, View] = defaultsTable.getDefaultView.asCFuture






  /**
    * Safely get the table for a name
    * @param t - the tablename
    */
  def lookupTable(t: TableName): SQLEither[ObjectTable] =
    tableLookup.getOrError(t, MissingTableName(t)).leftMap(SQLExtractError)

  /**
    * Safely get the relationTableName for a given relation
    * @param er - the tablename
    */
  def lookupRelation(er: ErasedRelationAttributes): SQLEither[RelationTable] =
    relationLookup.flatMap(_.getOrError(er, SQLErasedRelationMissing(er)))


  /**
    * read from the views table
    * @return
    */
  override def getViews: ConstrainedFuture[E, Set[View]] = viewsRegistry.getViews.asCFuture


  /**
    *  On startup, check that the tables are valid
    *  Returns an error if the tables are invalid
    */

  def validateTables(): SQLEither[Unit] =
    for {
      definedTables <- definedTables
      relations <- relationLookup
      auxiliaryTables = tableLookup.map{case (_, ot) => ot.auxTable.name -> ot.auxTable}
      constructionTables: Seq[(SQLTableName, SQLTable)] = List(viewsRegistry, commitsRegistry, viewsTable, defaultsTable).map(t => t.name -> t)
      validated <- EitherOps.sequence(
        for {
          (name, table) <-
          constructionTables ++ tableLookup ++ auxiliaryTables ++ relations // Order is important
        } yield table.validateOrCreate(definedTables)
      )
    } yield ()


  /**
    * Query to get the defined tables in DB
    * @return
    */

  private def definedTables: SQLEither[Set[SQLTableName]] = {
    val q = s"SELECT table_name FROM information_schema.tables WHERE table_schema='public';"
    reader.getTableNames(q)
  }


  /**
    * Query to get the defined constraints in the DB
    * @return
    */
 private def definedConstraints: SQLEither[Set[(SQLTableName, String)]] = {
    val q = "SELECT table_name, constraint_name  FROM information_schema.constraint_table_usage  WHERE table_schema = 'public';"
    reader.getConstraint(q).map {
      _.collect {
        case (name, constraint) if !(constraint.endsWith("pkey") || constraint.endsWith("fkey")) => (name, constraint)
      }
    }
 }

  /**
    * drop all existing user tables and contraints
    * Executed if we're expecting an empty DB
    */

  def freshen(): SQLEither[Unit] =
    for {
      constraints <- definedConstraints
      tables <- definedTables
      _ <- if (constraints.nonEmpty) writeBatchEither (
        constraints.map {
          case (constraintTable, constraintName) => s"ALTER TABLE $constraintTable DROP CONSTRAINT IF EXISTS $constraintName;"
        }
      ) else ().right
      _ <- if (tables.nonEmpty) doWriteEither(s"DROP TABLE ${tables.mkString(", ")} CASCADE;") else ().right
    } yield ()


  /**
    * Runs a write query in a future
    */
  def doWrite(query: String): SQLFuture[Unit] = SQLFutureE {doWriteEither(query)}

  /**
    * Runs a write query
    */
  def doWriteEither(query: String): SQLEither[Unit] = SQLEither {
    val fixedQuery = if (query.endsWith(";")) query else query + ";"
    val stmt = connection.createStatement()
    stmt.executeUpdate(fixedQuery)
  }

  /**
    * Runs a series of write commands in a future
    */
  def writeBatch(queries: TraversableOnce[String]): SQLFuture[Unit] = SQLFutureE {writeBatchEither(queries)}


  /**
    * Runs a series of write commandss
    */
  def writeBatchEither(queries: TraversableOnce[String]): SQLEither[Unit] = SQLEither {
    val stmt = connection.createStatement()
    for (query <- queries) stmt.addBatch(query)
    stmt.executeBatch()
  }
}