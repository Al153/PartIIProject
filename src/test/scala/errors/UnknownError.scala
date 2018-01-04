package errors

/**
  * Created by Al on 04/01/2018.
  */

case class UnknownError(t: Throwable) extends TestError {
  override def toString: String = "UnknownError\n"+ t.getStackTrace.mkString("\n")
}