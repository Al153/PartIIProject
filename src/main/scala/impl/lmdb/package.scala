package impl

import core.containers.ConstrainedFuture

import scalaz.\/

/**
  * Created by Al on 28/12/2017.
  */
package object lmdb {

  type LMDBEither[A] = errors.LMDBError \/ A
  type LMDBFuture[A] = ConstrainedFuture[errors.LMDBError, A]

}
