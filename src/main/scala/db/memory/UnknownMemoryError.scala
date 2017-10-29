package db.memory

import core.error.E

/**
  * Created by Al on 27/10/2017.
  */
case class UnknownMemoryError(t: Throwable) extends E {
  override def toString: String = "Unknown Memory throwable thrown \n" + t.getStackTrace.mkString("\n")
}
