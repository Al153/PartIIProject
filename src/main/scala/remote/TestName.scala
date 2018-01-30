package remote

case class TestName(name: String) extends AnyVal
object TestName {
  implicit class StringOps(u: String){
    def test: TestName = TestName(u)
  }
}