package impl.lmdb.tables.interfaces

import impl.lmdb.LMDBEither
import impl.lmdb.access.Storeable
import impl.lmdb._

/**
  * Created by Al on 29/12/2017.
  */
abstract class MutableCounter[A](implicit store: Storeable[A]) extends LMDBTable {
  initialise()
  /**
    * Transactional
    * @return
    */
  def getAndUpdate(): LMDBEither[A] = transactionalGetAndSet(path){
    a => LMDBEither(next(a))
  }
  protected def initialValue: A
  protected def next(a: A): A

  private def initialise(): Unit = put(path, initialValue)
}
