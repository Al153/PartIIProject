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
    List(viewsRegistry, commitsRegistry, viewsTable, defaultsTable).map(t => t.name -> t)



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
  private val relationLookup: SQLEither[Map[ErasedRelationAttributes, RelationTable]] =
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
    )).toMapE

  override def setDefaultView(view: View): ConstrainedFuture[E, Unit] = SQLFutureE(defaultsTable.setDefaultView(view)).asCFuture


  override def getDefaultView: ConstrainedFuture[E, View] = defaultsTable.getDefaultView.asCFuture






  /**
    * Safely get the table for a name
    * @param t - the tablename
    * @return
    */
  def lookupTable(t: TableName): SQLEither[ObjectTable] =
    tableLookup.getOrError(t, MissingTableName(t)).leftMap(SQLExtractError)


  def lookupRelation(er: ErasedRelationAttributes): SQLEither[RelationTable] =
    relationLookup.flatMap(_.getOrError(er, SQLRelationMissing(er)))


  // read from the views table
  override def getViews: ConstrainedFuture[E, Set[View]] = viewsRegistry.getViews.asCFuture




  // Returns an error if the tables are invalid
  def validateTables(): SQLFuture[Unit] =
    SQLFutureE(
      for {
        definedTables <- definedTables
        relations <- relationLookup
        auxiliaryTables = tableLookup.map{case (_, ot) => ot.auxTable.name -> ot.auxTable}
        validated <- EitherOps.sequence(
            for {
              (name, table) <-
                constructionTables ++ tableLookup ++ auxiliaryTables ++ relations // Order is important
            } yield table.validateOrCreate(definedTables)
          )
      } yield ()
    )





  private def definedTables: SQLEither[Set[SQLTableName]] = {
    val q = s"SELECT table_name FROM information_schema.tables WHERE table_schema='public';"
    reader.getTableNames(q)
  }

 private def definedConstraints: SQLEither[Set[(SQLTableName, String)]] = {
    val q = "SELECT table_name, constraint_name  FROM information_schema.constraint_table_usage  WHERE table_schema = 'public';"
    reader.getConstraint(q).map {
      _.collect {
        case (name, constraint) if !(constraint.endsWith("pkey") || constraint.endsWith("fkey")) => (name, constraint)
      }
    }
 }

  // drop all existing user tables
  def freshen(): SQLFuture[Unit] = SQLFutureE {
    for {
      constraints <- definedConstraints
      tables <- definedTables
      _ = println(tables)
      _ <- if (constraints.nonEmpty) writeBatchEither (
        constraints.map {
          case (constraintTable, constraintName) => s"ALTER TABLE $constraintTable DROP CONSTRAINT IF EXISTS $constraintName;"
        }
      ) else ().right
      _ <- if (tables.nonEmpty) doWriteEither(s"DROP TABLE ${tables.mkString(", ")} CASCADE;") else ().right
    } yield ()
  }


  def doWrite(query: String): SQLFuture[Unit] = SQLFutureE {doWriteEither(query)}

  def doWriteEither(query: String): SQLEither[Unit] = SQLEither {
    val fixedQuery = if (query.endsWith(";")) query else query + ";"
    println(fixedQuery)
    val stmt = connection.createStatement()
    stmt.executeUpdate(fixedQuery)
  }
  def writeBatch(queries: TraversableOnce[String]): SQLFuture[Unit] = SQLFutureE {writeBatchEither(queries)}

  def writeBatchEither(queries: TraversableOnce[String]): SQLEither[Unit] = SQLEither {

    println("Batch write = " + queries.mkString("\n\t\t"))
    val stmt = connection.createStatement()
    for (query <- queries) stmt.addBatch(query)
    stmt.executeBatch()
  }
}