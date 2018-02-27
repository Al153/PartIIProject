package remote.util

import core.user.containers.ConstrainedFuture
import core.user.dsl.{E, HasRecovery}
import core.user.interfaces.DBInstance
import core.user.schema.SchemaDescription
import core.utils._

import scala.concurrent.ExecutionContext

trait TestSpec[A] {
  def testName: TestName
  def batchSize: TestIndex
  def setup[ThisE <: E](d: DBInstance[ThisE])(implicit R: HasRecovery[ThisE], ec: ExecutionContext): ConstrainedFuture[ThisE, Unit]
  def test[ThisE <: E](d: DBInstance[ThisE])(index: TestIndex)(implicit R: HasRecovery[ThisE], ec: ExecutionContext): ConstrainedFuture[ThisE, A]
  def schema: SchemaDescription
  def ignoreBackends: Set[String]
  def canRun[ThisE <: E](oi: Option[(String, DBInstance[ThisE])]): Option[(String, DBInstance[ThisE])]
   = oi.flatMap{
    case (name, db) =>
      if (name in ignoreBackends) None
      else Some((name, db))
  }
}