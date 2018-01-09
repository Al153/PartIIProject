package core.user.dsl

import core.user.interfaces.DBInstance
import core.user.containers.{ConstrainedFuture, Operation}

import scala.concurrent.ExecutionContext
import scalaz.\/
/**
  * Created by Al on 17/10/2017.
  *
  * A number of helper methods/syntax for manipulating instances
  */
trait Opening {
  def using[A]
    (instance: DBInstance)
    (action: => Operation[E, A])
  : ConstrainedFuture[E, A] = {
    import instance.executionContext
    for {
      view <- instance.getDefaultView
      pair <- action.runView(view)
      (res, newView) = pair
      _ <- instance.setDefaultView(newView)
    } yield res
  }


  def usingView[A](instance: DBInstance, v: View)
                  (action: => Operation[E, A])
  : ConstrainedFuture[E, A] = {
    import instance._
    for {
      pair <- action.runView(v)
      (res, _) = pair
    } yield res
  }


  def writeToView(instance: DBInstance, v: View)
                 (action: => Operation[E, Unit])
  : ConstrainedFuture[E, View] = {
    import instance._
    for {
      pair <- action.runView(v)
      (_, newView) = pair
    } yield newView
  }

}
