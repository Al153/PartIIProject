package unit

import core.user.interfaces.DBBackend
import impl.lmdb.logjoins.LMDB
import unit.suites.FullSuite

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits

/**
  * Created by Al on 10/02/2018.
  */
class LogJoinsTest extends FullSuite {
  override val backend: DBBackend = LMDB
  implicit override val ec: ExecutionContext = Implicits.global
}