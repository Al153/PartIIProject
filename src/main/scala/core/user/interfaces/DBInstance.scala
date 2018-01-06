package core.user.interfaces

import core.user.containers.ConstrainedFuture
import core.user.dsl.{E, View}

import scala.concurrent.ExecutionContext
import scalaz.\/

/**
  * Created by Al on 29/10/2017.
  */
trait DBInstance {
  implicit val executionContext: ExecutionContext
  def executor: DBExecutor


  def setDefaultView(view: View): ConstrainedFuture[E, Unit]
  def getDefaultView: ConstrainedFuture[E, View]
  def getViews: ConstrainedFuture[E, Set[View]]

  def close(): Unit
}

