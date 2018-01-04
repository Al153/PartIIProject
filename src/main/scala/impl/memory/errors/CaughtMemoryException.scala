package impl.memory.errors

import impl.lmdb.errors.LMDBError

/**
  * Created by Al on 04/01/2018.
  */
case class CaughtMemoryException(e: Throwable) extends MemoryError {
  override def toString: String = "CAUGHT UNKNOWN MEMORY EXCEPTION: " + e.toString + "\n" + e.getStackTrace.mkString("\n")
}
