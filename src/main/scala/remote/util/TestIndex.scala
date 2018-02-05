package remote.util

case class TestIndex(i: Int) extends AnyVal
object TestIndex {
  implicit class IntOps(i: Int) {
    def to(ti: TestIndex): IndexedSeq[TestIndex] =
      for (j <- i to ti.i) yield TestIndex(j)


    val tests: TestIndex = new TestIndex(i)
  }
}