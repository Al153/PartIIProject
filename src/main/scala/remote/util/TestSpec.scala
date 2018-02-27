package remote.util

import core.user.containers.ConstrainedFuture
import core.user.dsl.E
import core.user.interfaces.DBInstance
import core.user.schema.SchemaDescription
import core.utils._

import scala.concurrent.ExecutionContext

trait TestSpec[A] {
  def testName: TestName
  def batchSize: TestIndex
  def setup(d: DBInstance[_ <: E])(implicit ec: ExecutionContext): ConstrainedFuture[E, Unit]
  def test(d: DBInstance[_ <: E])(index: TestIndex)(implicit ec: ExecutionContext): ConstrainedFuture[E, A]
  def schema: SchemaDescription
  def ignoreBackends: Set[String]
  def getTestable(tests: Seq[(String, DBInstance[_ <: E])]): Seq[(String, DBInstance[_ <: E])] =
    tests.filter {case (name, _) => name notIn ignoreBackends}
}