package impl.lmdb

import core.user.interfaces.DBExecutor
import impl.lmdb.methods.Methods

/**
  * Created by Al on 12/12/2017.
  *
  *  Executor implementation in the Methods trait
  */

class LMDBExecutor(implicit val instance: LMDBInstance) extends DBExecutor with Methods
