package impl.lmdb.tables.interfaces

import impl.lmdb.{LMDBEither, _}
import impl.lmdb.access.{Key, Storeable}

/**
  * Created by Al on 29/12/2017.
  *
  * A mutable counter is a transactional counter in the database for getting unique values for objects
  */
abstract class MutableCounter[A](key: Key)(implicit store: Storeable[A]) extends LMDBTable {


  /**
    * Transactional update
    * @return
    */
  def getAndUpdate(): LMDBEither[A] = transactionalGetAndSet(key, db){
    a => LMDBEither(next(a))
  }

  /**
    * Need to define a starting value (the first that will be defined)
    * @return
    */
  protected def initialValue: A

  /**
    * Need to define a successor function
    * @param a - old value
    * @return
    */
  protected def next(a: A): A

  /**
    * initialise should be called if the database is a new one
    */
  def initialise(): LMDBEither[Unit] = put(key, initialValue, db)
}
