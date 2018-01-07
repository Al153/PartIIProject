package core.user.interfaces

import core.user.containers.ConstrainedFuture
import core.user.dsl.{E, View}
import core.user.schema.SchemaDescription

import scala.concurrent.ExecutionContext

/**
  * Created by Al on 29/10/2017.
  */
trait DBInstance {
  implicit def executionContext: ExecutionContext
  def schema: SchemaDescription
  def executor: DBExecutor


  def setDefaultView(view: View): ConstrainedFuture[E, Unit]
  def getDefaultView: ConstrainedFuture[E, View]
  def getViews: ConstrainedFuture[E, Set[View]]

  def close(): Unit
}

