import core.user.dsl.E
import org.slf4j.{Logger, LoggerFactory}

/**
  * Created by Al on 28/01/2018.
  */
package object remote {
  implicit val logger: Logger = LoggerFactory.getLogger(classOf[remote])
  val formatter = java.text.NumberFormat.getIntegerInstance
  /**
    * Returns \bot but has any type
    * @param e - error to throw
    * @tparam A - type to shoehorn to
    * @return
    */
  def errorThrowable[A](e: E): A = throw new Throwable {
    override def toString: String = e.toString
  }

  def ns(l: Long): String = formatter.format(l)+"ns"
}
