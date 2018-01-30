package remote

class TestSpec(val testName: TestName, val batchSize: TestIndex)

object TestSpec {
  def apply(testName: String, batchSize: Int): TestSpec =
    new TestSpec(TestName(testName), TestIndex(batchSize))
}