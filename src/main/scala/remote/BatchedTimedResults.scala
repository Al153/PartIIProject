package remote

case class BatchedTimedResults[A](tas: Seq[TimeResult[A]]) {
  def testName: String = tas.headOption.fold("NoTests"){ta => ta.instance.testBackend}
  def length: Int = tas.size
  def fullTime: Long = tas.foldLeft(0l){_ + _.ns}
}