package core.user.interfaces

import core.user.containers.ConstrainedFuture
import core.user.dsl.{E, View}
import core.user.schema.SchemaDescription

import scala.concurrent.ExecutionContext

/**
  * Created by Al on 29/10/2017.
  *
  * An open Database connection
  */
trait DBInstance {
  /**
    * The bound [[ExecutionContext]]
    */
  implicit def executionContext: ExecutionContext

  /**
    * The schema the DB was instantiated with
    * @return
    */
  def schema: SchemaDescription

  /**
    * The database's executor
    * @return
    */
  def executor: DBExecutor


  /**
    * Set the default view of the database
    */
  def setDefaultView(view: View): ConstrainedFuture[E, Unit]

  /**
    * Get hold of the default view
    */

  def getDefaultView: ConstrainedFuture[E, View]

  /**
    * Get all available views from the Database
    */

  def getViews: ConstrainedFuture[E, Set[View]]

  /**
    * Close the database
    */
  def close(): Unit
}

