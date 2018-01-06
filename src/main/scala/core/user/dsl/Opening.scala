package core.user.dsl

import core.user.interfaces.DBInstance
import core.user.containers.{ConstrainedFuture, Operation}

import scala.concurrent.ExecutionContext
// todo: Do we want to get rid of implicit executionContext?
/**
  * Created by Al on 07/10/2017.
  */
trait Opening {
  def using[A]
    (edb: ConstrainedFuture[E, DBInstance])
    (action: DBInstance => Operation[E, A])
  : ConstrainedFuture[E, A] = {
    import edb.ec
    for {
      instance <- edb
      view <- instance.getDefaultView
      pair <- action(instance).runView(view)
      (res, newView) = pair
      _ <- instance.setDefaultView(newView)
    } yield res
  }


  def usingView[A](instance: DBInstance, v: View)
                  (action: DBInstance => Operation[E, A])
  : ConstrainedFuture[E, A] = {
    import instance._
    for {
      pair <- action(instance).runView(v)
      (res, _) = pair
    } yield res
  }


  def writeToView(instance: DBInstance, v: View)
                 (action: DBInstance => Operation[E, Unit])
  : ConstrainedFuture[E, View] = {
    import instance._
    for {
      pair <- action(instance).runView(v)
      (_, newView) = pair
    } yield newView
  }

}
