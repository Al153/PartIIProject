package remote

import core.user.containers.ConstrainedFuture
import core.user.dsl.E
import core.user.interfaces.DBInstance
import core.user.schema.SchemaDescription

import scala.concurrent.ExecutionContext

trait TestSpec[A] {
  def testName: TestName
  def batchSize: TestIndex
  def setup(d: DBInstance)(implicit ec: ExecutionContext): ConstrainedFuture[E, Unit]
  def test(d: DBInstance)(index: TestIndex)(implicit ec: ExecutionContext): ConstrainedFuture[E, A]
  def schema: SchemaDescription
}