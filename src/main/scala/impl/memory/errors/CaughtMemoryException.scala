package impl.memory.errors

import impl.lmdb.common.errors.LMDBError

/**
  * Created by Al on 04/01/2018.
  *
  * Error which catches exceptions
  */
case class CaughtMemoryException(e: Throwable) extends MemoryError {
  override def toString: String = "CAUGHT UNKNOWN MEMORY EXCEPTION: " + e.toString + "\n" + e.getStackTrace.mkString("\n")
}
