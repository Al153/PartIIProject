package remote.util

case class BatchedTimedResults[A](tas: Seq[TimeResult[A]]) {
  def testName: TestName = tas.headOption.fold(TestName("NoTests")){ta => ta.instance.testName}
  def backend: String = tas.headOption.fold("NoTests"){ ta => ta.instance.testBackend}
  def length: Int = tas.size
  def fullTime: Long = tas.foldLeft(0l){_ + _.ns}
}