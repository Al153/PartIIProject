package remote

case class TimeResult[A](instance: TestInstance, ns: Long, a: A)