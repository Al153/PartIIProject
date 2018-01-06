package impl.sql

import java.sql.{Connection, DriverManager}
import java.util.Properties

import core.user.interfaces._
import core.user.containers.ConstrainedFuture
import core.user.dsl.{DBDir, DatabaseAddress, E, Empty}
import core.user.schema.SchemaDescription

import scala.concurrent.ExecutionContext

/**
  * To use postgreSQL
  */

object SQLDB extends DBBackend {
  override def open(
                     address: DatabaseAddress,
                     schema: SchemaDescription
                   )(implicit e: ExecutionContext): ConstrainedFuture[E, DBInstance] = {
    val r = for {
      conn <- openConnection(address, schema)
      instance = new SQLInstance(conn, schema)
      _ <- if (address.isInstanceOf[Empty.type]) instance.freshen() else SQLFuture(())
      _ <- instance.validateTables()
    } yield instance: DBInstance
    r.asCFuture
  }

  // Opens a database connection somehow

  private def openConnection(
                              address: DatabaseAddress,
                              schema: SchemaDescription
                            )(implicit ec: ExecutionContext): SQLFuture[Connection] =
    SQLFuture {
      address match {
        case DBDir(path, user, password) =>
          val jdbcUrl = s"jdbc:postgresql://localhost/${path.toString}"
          val props = new Properties()
          props.setProperty("user", user)
          props.setProperty("password", password)
          props.setProperty("ssl", "false")
          DriverManager.getConnection(jdbcUrl, props)
        case Empty =>
          val jdbcUrl = s"jdbc:postgresql://localhost/postgres"
          val props = new Properties()
          props.setProperty("user", "postgres")
          props.setProperty("password", " ")
          props.setProperty("ssl", "false")
          DriverManager.getConnection(jdbcUrl, props)
    }}


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