package core.user.dsl

import core.user.interfaces.DBInstance
import core.user.containers.{ConstrainedFuture, Operation}

import scala.concurrent.ExecutionContext
import scalaz.\/
/**
  * Created by Al on 17/10/2017.
  *
  * A number of helper methods/syntax for manipulating instances, in an error-free way
  */
trait Opening {
  /**
    * Run an operation against a [[DBInstance]]'s default view, setting the new default view.
    * Not really threadsafe (T1 runs a using then T2 runs a using before T1 writes its default, so T1's effects are lost)
    *
    * @param instance - instance to run on
    * @param action - read/write operation
    * @tparam A - type returned
    * @return
    */
  def using[A]
    (instance: DBInstance[_ <: E])
    (action: => Operation[_ <: E, A])
  : ConstrainedFuture[E, A] = {
    import instance.executionContext
    for {
      view <- instance.getDefaultView.eraseError
      pair <- action.runView(view).eraseError
      (res, newView) = pair
      _ <- instance.setDefaultView(newView).eraseError
    } yield res
  }

  /**
    * Read a value without setting the default view
    */
  def readDefault[A]
    (instance: DBInstance[_ <: E])
    (action: => Operation[_ <: E, A])
  : ConstrainedFuture[E, A] = {
    import instance.executionContext
    for {
      view <- instance.getDefaultView.eraseError
      pair <- action.runView(view).eraseError
      (res, _) = pair
    } yield res
  }


  /**
    * Runs a read operation against an instance, discarding any new views created (they are unreachable)
    * @param instance - instance to run on
    * @param v - view to use
    * @param action - action
    * @tparam A - type returned
    * @return
    */
  // todo: is instance param needed?
  def usingView[A](instance: DBInstance[_ <: E], v: View)
                  (action: => Operation[_ <: E, A])
  : ConstrainedFuture[E, A] = {
    import instance._
    for {
      pair <- action.runView(v).eraseError
      (res, _) = pair
    } yield res
  }

  /**
    * Runs a write operation on a view, returning the new view created
    * @param instance - instance to run on
    * @param v - view to start with
    * @param action - action to run
    */
  def writeToView(instance: DBInstance[_ <: E], v: View)
                 (action: => Operation[_ <: E, Unit])
  : ConstrainedFuture[E, View] = {
    import instance._
    for {
      pair <- action.runView(v).eraseError
      (_, newView) = pair
    } yield newView
  }

}
