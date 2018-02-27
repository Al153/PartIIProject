package core.user.dsl

import core.user.containers.{ConstrainedFuture, Operation}
import core.user.interfaces.DBInstance
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
  def using[E1 <: E, A]
    (instance: DBInstance[E1])
    (action: => Operation[E1, A])
  : ConstrainedFuture[E1, A] = {
    for {
      view <- instance.getDefaultView
      pair <- action.runView(view)
      (res, newView) = pair
      _ <- instance.setDefaultView(newView)
    } yield res
  }

  /**
    * Read a value without setting the default view
    */
  def readDefault[E1 <: E, A]
    (instance: DBInstance[E1])
    (action: => Operation[E1, A])
  : ConstrainedFuture[E1, A] = {
    for {
      view <- instance.getDefaultView
      pair <- action.runView(view)
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
  def usingView[E1 <: E, A](instance: DBInstance[E1], v: ViewId)
                  (action: => Operation[E1, A])
  : ConstrainedFuture[E1, A] = {
    for {
      pair <- action.runView(v)
      (res, _) = pair
    } yield res
  }

  /**
    * Runs a write operation on a view, returning the new view created
    * @param instance - instance to run on
    * @param v - view to start with
    * @param action - action to run
    */
  def writeToView[E1 <: E](instance: DBInstance[E1], v: ViewId)
                 (action: => Operation[E1, Unit])
  : ConstrainedFuture[E1, ViewId] = {
    for {
      pair <- action.runView(v)
      (_, newView) = pair
    } yield newView
  }
}
