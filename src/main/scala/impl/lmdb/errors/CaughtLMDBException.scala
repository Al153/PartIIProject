package impl.lmdb.errors

/**
  * Created by Al on 28/12/2017.
  */
case class CaughtLMDBException(e: Throwable) extends LMDBError {
  override def toString: String = "CAUGHT UNKNOWN LMDB EXCEPTION: " + e.toString + "\n" + e.getStackTrace.mkString("\n")
}