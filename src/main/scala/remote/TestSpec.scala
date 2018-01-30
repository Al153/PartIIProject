package remote

import core.user.interfaces.DBBackend

trait TestSpec {
  def batchSize: TestIndex
  def referenceImplementation: DBBackend
  def testImplementations: Vector[(String, DBBackend)]
}