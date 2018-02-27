package core.user.interfaces

import core.user.containers.ConstrainedFuture
import core.user.dsl.{E, ViewId}
import core.user.schema.SchemaDescription
import core.utils.Logged

import scala.concurrent.ExecutionContext

/**
  * Created by Al on 29/10/2017.
  *
  * An open Database connection
  */
trait DBInstance[E <: core.user.dsl.E] extends Logged{
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
  def executor: DBExecutor[E]


  /**
    * Set the default view of the database
    */
  def setDefaultView(view: ViewId): ConstrainedFuture[E, Unit]

  /**
    * Get hold of the default view
    */

  def getDefaultView: ConstrainedFuture[E, ViewId]

  /**
    * Get all available views from the Database
    */

  def getViews: ConstrainedFuture[E, Set[ViewId]]

  /**
    * Close the database
    */
  def close(): Unit
}

