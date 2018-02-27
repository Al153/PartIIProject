package impl.lmdb.common.interfaces

import core.user.interfaces.DBExecutor
import impl.lmdb.common.errors.LMDBError

trait LMDBExecutor extends DBExecutor[LMDBError]