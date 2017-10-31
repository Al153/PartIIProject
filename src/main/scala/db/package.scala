import core.containers.{ConstrainedFuture, Operation}
import core.error.E
import db.interfaces.DBInstance

/**
  * Created by Al on 07/10/2017.
  */
package object db {
  def using[A](edb: ConstrainedFuture[E, DBInstance])(action: DBInstance => Operation[E, A]): ConstrainedFuture[E, A] = ???
}
