package impl.lmdb.errors

import impl.lmdb.access.ObjId

/**
  * Created by Al on 01/01/2018.
  *
  * Error thrown when a value we want to insert does not appear in the pregenerated index
  * (ie something has gone very wrong)
  */
case class LMDBMissingInsert[A](a: A, lookup: Map[A, ObjId]) extends LMDBError {

}
