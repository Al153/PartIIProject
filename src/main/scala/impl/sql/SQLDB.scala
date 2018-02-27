package impl.sql

import java.sql.{Connection, DriverManager}
import java.util.Properties

import core.user.dsl.{DBDir, DatabaseAddress, E, Empty}
import core.user.interfaces._
import core.user.schema.SchemaDescription
import impl.sql.errors.SQLError

import scala.concurrent.ExecutionContext
import scalaz.Scalaz._
import scalaz._

/**
  * PostgreSQL based DB implementation
  */

object SQLDB extends DBBackend[SQLError] {
  /**
    * Open an SQLInstance
    * @param address - Address to open
    * @param schema - Schema to open with
    * @return a [[SQLInstance]]
    */
  override def open(
                     address: DatabaseAddress,
                     schema: SchemaDescription
                   )(implicit e: ExecutionContext): SQLEither[DBInstance[SQLError]] = try {
    // start a connection
    val conn = openConnection(address)
    val instance = new SQLInstance(conn, schema)
    for {
      _ <- if (address.isInstanceOf[Empty.type]) instance.freshen() else ().right
      _ <- instance.validateTables()
    } yield instance: DBInstance[SQLError]
  } catch {case e: Throwable => errors.SQLRecovery.recover(e).left}



  /**
    * Open an SQL connection
   */

  private def openConnection(
                              address: DatabaseAddress
                            )(implicit ec: ExecutionContext): Connection =
      address match {
        case DBDir(path, user, password) =>

          /**
            * Open an SQL instance at a given place
            */
          val jdbcUrl = s"jdbc:postgresql://localhost/${path.toString}"
          val props = new Properties()
          props.setProperty("user", user)
          props.setProperty("password", password)
          props.setProperty("ssl", "false")
          DriverManager.getConnection(jdbcUrl, props)
        case Empty =>

          /**
            * Open default SQL instance
            */
          val jdbcUrl = s"jdbc:postgresql://localhost/postgres"
          val props = new Properties()
          props.setProperty("user", "postgres")
          props.setProperty("password", " ")
          props.setProperty("ssl", "false")
          DriverManager.getConnection(jdbcUrl, props)
    }

  /**
    * Global names to use when rendering queries
    */

  val leftmostTable = "left_table"
  val rightmostTable = "right_table"
  val mainQuery = "main_query"
  val singleTable = "single_table"

  val temporaryView = "temporary_views_table"




}