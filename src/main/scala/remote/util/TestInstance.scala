package remote.util

case class TestInstance(testName: TestName, testBackend: String, testIndex: TestIndex) {
  override def toString: String = s"impl: $testBackend, test:${testName.name}, index:${testIndex.i}"
}
object TestInstance {
  import TestIndex._
  def apply(spec: TestSpec[_], testBackend: String): Seq[TestInstance] =
    for (i <- 1 to spec.batchSize) yield TestInstance(spec.testName, testBackend, i)
}