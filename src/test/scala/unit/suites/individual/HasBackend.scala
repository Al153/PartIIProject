package unit.suites.individual

import core.user.containers.ConstrainedFuture
import core.user.dsl.{E, Empty}
import core.user.interfaces.{DBBackend, DBInstance}
import unit.{description, errorThrowable}

import scala.concurrent.{Await, ExecutionContext}
import concurrent.duration._


trait HasBackend {
  def backend: DBBackend
  implicit def ec: ExecutionContext

  def runTest(t: DBInstance => E ConstrainedFuture Unit): Unit =
    backend
      .open(Empty, description)
      .fold(errorThrowable, i =>
        Await.result(
          t(i).run , 10.seconds
        ).fold(errorThrowable, identity)
    )
}