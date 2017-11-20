package impl.sql

import core.backend.interfaces._
import core.containers.ConstrainedFuture
import core.error.E
import core.schema.SchemaDescription
import org.h2.engine.Session

import scala.concurrent.ExecutionContext

object SQLDB extends DBBackend {
  override def open(
                     address: DatabaseAddress,
                     schema: SchemaDescription
                   )(implicit e: ExecutionContext): ConstrainedFuture[E, DBInstance] = {
    // Step 1: open connection
    val connection: Session = openConnection(address, schema)

    // step 2: Validate tables
    for {
      _ <- validateTables(connection)

    } yield new SQLInstance(???)
  }

  // Opens a database connection somehow

  def openConnection(address: DatabaseAddress, schema: SchemaDescription): Session = address match {
    case DBUrl(url) => ???
    case DBDir(path) => ???
    case Empty => ???
  }

  // Returns an error if the tables are invalid
  def validateTables(session: Session): ConstrainedFuture[E, Unit] = ???

  val objId = "obj_id"
  val leftId = "left_id"
  val rightId = "right_id"
  val leftmostTable = "left_table"
  val rightmostTable = "right_table"
  val mainQuery = "main_query"
  val viewId = "view_id"
  def column(i: Int): String = "col_" + i
  def leftColumn(i: Int): String = "left_col_" + i
  def rightColumn(i: Int): String = "right_col_" + i



}