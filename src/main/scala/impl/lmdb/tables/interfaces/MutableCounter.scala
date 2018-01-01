package impl.lmdb.tables.interfaces

import impl.lmdb.LMDBEither
import impl.lmdb.access.Storeable

/**
  * Created by Al on 29/12/2017.
  */
abstract class MutableCounter[A](implicit store: Storeable[A]) extends LMDBTable {
  /**
    * Transactional
    * @return
    */
  def getAndUpdate(): LMDBEither[A] = ???
}
