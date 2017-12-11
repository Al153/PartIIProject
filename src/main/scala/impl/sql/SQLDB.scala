package impl.sql

import java.sql.{Connection, DriverManager}
import java.util.Properties

import core.backend.interfaces._
import core.containers.ConstrainedFuture
import core.error.E
import core.schema.SchemaDescription
import impl.sql.errors.SQLError

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
      instance = new SQLInstance(conn, schema)
      _ <- instance.validateTables()
    } yield instance: DBInstance
  }

  // Opens a database connection somehow

  private def openConnection(address: DatabaseAddress, schema: SchemaDescription)(implicit ec: ExecutionContext): E ConstrainedFuture Connection = ConstrainedFuture.point[E, Connection] {
    address match {
      case DBUrl(url, user, password) =>
        val jdbcUrl = s"jdbc:postgresql://${url.toString}"
        val props = new Properties()
        props.setProperty("user", user)
        props.setProperty("password", password)
        props.setProperty("ssl", "true")
        DriverManager.getConnection(jdbcUrl, props)
      case DBDir(path, user, password) =>
        val jdbcUrl = s"jdbc:postgresql://localhost/${path.toString}"
        val props = new Properties()
        props.setProperty("user", user)
        props.setProperty("password", password)
        props.setProperty("ssl", "true")
        DriverManager.getConnection(jdbcUrl, props)
      case Empty => ???
    }} (errors.recoverSQLException)


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