package core

import core.containers.{ConstrainedFuture, Operation}
import core.error.E
import core.backend.interfaces.DBInstance
import core.view.View

import scala.concurrent.ExecutionContext

/**
  * Created by Al on 07/10/2017.
  */
package object backend {
  def using[A]
    (edb: ConstrainedFuture[E, DBInstance])
    (action: DBInstance => Operation[E, A])
    (implicit ec: ExecutionContext)
  : ConstrainedFuture[E, A] =
    for {
      instance <- edb
      view <- instance.getDefaultView
      pair <- action(instance).runView(view)
      (res, newView) = pair
      _ <- instance.setDefaultView(newView)
    } yield res

  def usingView[A](instance: DBInstance, v: View)
                  (action: DBInstance => Operation[E, A])
                  (implicit e: ExecutionContext): ConstrainedFuture[E, A] =
    for {
      pair <- action(instance).runView(v)
      (res, _) = pair
    } yield res

  def writeToView(instance: DBInstance, v: View)
                 (action: DBInstance => Operation[E, Unit])
                 (implicit e: ExecutionContext): ConstrainedFuture[E, View] =
    for {
      pair <- action(instance).runView(v)
      (_, newView) = pair
    } yield newView
}
