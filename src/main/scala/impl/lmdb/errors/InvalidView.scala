package impl.lmdb.errors

import core.view.View

/**
  * Created by Al on 02/01/2018.
  */
case class InvalidView(v: View) extends LMDBError
