package core.error

/**
  * Created by Al on 03/10/2017.
  *
  * Error class
  */
trait E {

}

case class UnknownError(t: Throwable) extends E {
  override def toString: String = "UknownError\n"+ t.getStackTrace.mkString("\n")
}
