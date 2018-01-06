package unit

import core.user.interfaces.DBBackend
import impl.sql.SQLDB
import unit.suites._
import unit.suites.individual._

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits

/**
  * Created by Al on 18/12/2017.
  */
class SQLTests extends FullSuite {
  override val backend: DBBackend = SQLDB
  implicit override val ec: ExecutionContext = Implicits.global
}