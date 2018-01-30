package remote

case class TestInstance(testBackend: String, testIndex: TestIndex)
object TestInstance {
  import TestIndex._
  def apply(testBackend: String)(topIndex: TestIndex): Seq[TestInstance] =
    for (i <- 1 to topIndex) yield TestInstance(testBackend, i)
}