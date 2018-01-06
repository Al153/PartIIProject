package impl.lmdb.errors

import core.user.dsl.View

/**
  * Created by Al on 02/01/2018.
  */
case class InvalidView(v: View) extends LMDBError
