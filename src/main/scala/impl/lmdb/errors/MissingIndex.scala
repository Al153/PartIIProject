package impl.lmdb.errors

import impl.lmdb.access.ObjId

/**
  * Created by Al on 01/01/2018.
  */
case class MissingIndex[A](id: ObjId, index: Map[ObjId, A]) extends LMDBError {

}
