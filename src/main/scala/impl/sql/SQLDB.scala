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
    *   todo: select the right fields of left
    *
    *  Rel =>
    *      select (left_id as left_id, right_id as right_id) from $rel_x
    *
    *
    *  RevRel =>
    *      select (left_id as right_id, right_id as left_id) from $rel_x
    *
    *  Id =>
    *     select (id as left_id, id as right_id) from $table
    *
    *  Or(a, b) =>
    *    with
    *       with A as ($recurse(a)),
    *       with B as ($recurse(b))
    *       (A union B)
    *
    * And(a, b) =>
    *     with A as ($recurse(a)),
    *          B as ($recurse(b))
    *       (A intersect B)
    *
    * Chain(a, b)
    *     with A as ($recurse(a)),
    *          B as ($recurse(b))
    *     Select(A.left_id as left_id, B.right_id as right_id) from (A join B on A.right_id = B.left_id)
    *
    *
    * AndSingle(rel, b) =>
    *     with A as ($recurse(a)),
    *          B as ($recurse(b))
    *       select(A.left_id as left_id, A.right_id as right_id) from (A join B on A.right_id = B.left_id)
    *
    *
    * Narrow(rel, pattern) =>
    *
    *       with A as ($recurse(rel)),
    *            B as ($find(pattern))
    *          select(A.left_id as left_id, A.right_id as right_id) from (A join B on A.right_id = B.left_id)
    *
    * Distinct(a) =>
    *     with A as ($recurse(a))
    *     select (left_id, right_id) from A where (left_id != right_id)
    *
    * // These are a little harder to optimise
    *
    * Upto(n, rel) =>
    *
    * Exactly(n, rel) =>
    *
    * Between(low, high, rel) =>
    *
    * Atleast(n, rel) =>
    */

}