package unit.suites.individual

import core.user.containers.ConstrainedFuture
import core.user.dsl.{E, Empty, HasRecovery}
import core.user.interfaces.{DBBackend, DBInstance}
import unit.{description, errorThrowable}

import scala.concurrent.{Await, ExecutionContext}
import concurrent.duration._


trait HasBackend[E1 <: E] {
  def backend: DBBackend[E1]
  implicit def ec: ExecutionContext
  implicit def R: HasRecovery[E1]

  def runTest(t: DBInstance[E1] => E1 ConstrainedFuture Unit): Unit =
    backend
      .open(Empty, description)
      .fold(errorThrowable, i =>
        try {
          val res = Await.result(
            t(i).run , 10.seconds
          ).fold(errorThrowable, identity)

          res
        } finally {
          if (i != null) i.close()
        }
      )
}