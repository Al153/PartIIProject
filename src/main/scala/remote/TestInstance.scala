package remote

case class TestInstance(testName: TestName, testBackend: String, testIndex: TestIndex) {
  override def toString: String = s"impl: $testBackend, test:${testName.name}, index:${testIndex.i}"
}
object TestInstance {
  import TestIndex._
  def apply(name: TestName,testBackend: String)(topIndex: TestIndex): Seq[TestInstance] =
    for (i <- 1 to topIndex) yield TestInstance(name, testBackend, i)
}