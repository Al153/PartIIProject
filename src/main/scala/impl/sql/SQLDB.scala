package impl.sql

import java.sql.{Connection, DriverManager}
import java.util.Properties

import core.backend.interfaces._
import core.containers.ConstrainedFuture
import core.error.E
import core.schema.SchemaDescription

import scalaz._
import Scalaz._
import scala.concurrent.ExecutionContext

/**
  * To use postgreSQL
  */

object SQLDB extends DBBackend {
  override def open(
                     address: DatabaseAddress,
                     schema: SchemaDescription
                   )(implicit e: ExecutionContext): ConstrainedFuture[E, DBInstance] = {
    // Step 1: open connection


    // step 2: Validate tables
    for {
      conn <- openConnection(address, schema)
      _ <- validateTables(???)

    } yield new SQLInstance(???, ???)
  }

  // Opens a database connection somehow

  private def openConnection(address: DatabaseAddress, schema: SchemaDescription): E \/ Connection = try {
    address match {
      case DBUrl(url, user, password) =>
        val jdbcUrl = s"jdbc:postgresql://${url.toString}"
        val props = new Properties()
        props.setProperty("user", user)
        props.setProperty("password", password)
        props.setProperty("ssl", "true")
        DriverManager.getConnection(jdbcUrl, props).right
      case DBDir(path, user, password) =>
        val port = ???
        val jdbcUrl = s"jdbc:postgresql://localhost:$port/${path.toString}"
        val props = new Properties()
        props.setProperty("user", user)
        props.setProperty("password", password)
        props.setProperty("ssl", "true")
        DriverManager.getConnection(jdbcUrl, props).right
      case Empty => ???
    }
  } catch {case e: Throwable => errors.recoverSQLException(e).left}

    // Returns an error if the tables are invalid
    def validateTables(session: Any): ConstrainedFuture[E, Unit] = ???


    val leftmostTable = "left_table"
    val rightmostTable = "right_table"
    val mainQuery = "main_query"
    val singleTable = "single_table"

    val temporaryView = "temporary_views_table"

  /**
    * SQL Tables that should exist:
    *
    * ViewRegistry
    *
    *   ViewId: SQLPRimaryRef
    *
    * CommitRegistry
    *
    *   CommitId: SQLPrimaryRef
    *
    * ViewsTable
    *
    *   ViewId: SQLForeignRef(ViewRegistry),
    *   CommitId: SQLForeignRef(CommitRegistry)
    *
    *
    * For each object class:
    *
    * Object table
    *   CommitId: SQLForeignRef(CommitRegistry)
    *   ObjectId: SQLPrimaryRef
    *   col_1: Translate(type1)
    *   col_2: Translate(type2)
    *   .
    *   .
    *   .
    *   col_n: Translate(typen)
    *
    *
    *
    *
    * For each relation:
    *
    * RelationTable
    *   LeftId: SQLForeignRef(ObjectTableName)
    *   CommitId: SQLForeignRef(ObjectTableName)
    *   RightId: SQLForeignRef(ObjectTableName)
    *
    *
    */


}