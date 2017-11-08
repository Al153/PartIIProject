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

  /**
    * Idea for composition of queries
    *
    * - Chain up queries on the id column using join
    * - Always given query for lhs and commit table
    * - wrapped in: ```
    *   with commit as (
    *     select commit_id from views where view_id = id
    *     ), $rel_1 as (select (left_id, right_id) from $rel1 join commit on commit_id = commit_id)
    *     .
    *     .
    *     .
    * ```
    *
    *  Rel =>
    *      with LEFT as ($left)
    *      select (Left, right_id as id) from LEFT join
    *       $rel_x on LEFT.id = left_id
    *
    */

}