package impl.lmdb.errors

import impl.lmdb.access.ObjId

/**
  * Created by Al on 01/01/2018.
  */
case class LMDBMissingInsert[A](a: A, lookup: Map[A, ObjId]) extends LMDBError {

}
