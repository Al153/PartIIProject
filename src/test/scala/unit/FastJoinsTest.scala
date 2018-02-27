package unit

import core.user.dsl.HasRecovery
import core.user.interfaces.DBBackend
import impl.lmdb.common.errors.{LMDBError, LMDBRecover}
import impl.lmdb.fastjoins.LMDB
import unit.suites.FullSuite

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits

/**
  * Created by Al on 10/02/2018.
  */
class FastJoinsTest extends FullSuite[LMDBError] {
  override val backend: DBBackend[LMDBError] = LMDB
  implicit override val ec: ExecutionContext = Implicits.global

  override implicit def R: HasRecovery[LMDBError] = LMDBRecover
}