package impl.lmdb.tables.interfaces

import impl.lmdb.access.{Commit, Storeable}
import impl.lmdb.containers.SingleExtractor

/**
  * Created by Al on 29/12/2017.
  */
abstract class MutableCounter[A](implicit store: Storeable[A]) extends LMDBTable {
  /**
    * Transactional
    * @return
    */
  def getAndUpdate(): SingleExtractor[Commit] = ???
}
