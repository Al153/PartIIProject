import core.containers.{ConstrainedFuture, Operation}
import core.error.E
import db.interfaces.DBInstance
import schema.SchemaDescription

import scala.concurrent.ExecutionContext

/**
  * Created by Al on 07/10/2017.
  */
package object db {
  def using[A]
    (edb: ConstrainedFuture[E, DBInstance])
    (action: DBInstance => Operation[E, A])
    (implicit ec: ExecutionContext)
  : ConstrainedFuture[E, A] =
    for {
      instance <- edb
      view <- ConstrainedFuture.eitherR(instance.getDefaultView)
      pair <- action(instance).runView(view)
      (res, newView) = pair
      _ <- ConstrainedFuture.eitherR(instance.setDefaultView(newView))
    } yield res
}
