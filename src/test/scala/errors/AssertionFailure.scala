package errors

import core.error.E

/**
  * Created by Al on 04/01/2018.
  */


case class AssertionFailure(e: Throwable, msg: String) extends TestError {
  override def toString: String = s"AssertionFailure:\n$msg\n${e.getMessage}"
}


