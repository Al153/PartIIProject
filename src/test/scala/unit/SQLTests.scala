package unit

import core.user.dsl.HasRecovery
import core.user.interfaces.DBBackend
import impl.sql.SQLDB
import impl.sql.errors.{SQLError, SQLRecovery}
import unit.suites._
import unit.suites.individual._

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits

/**
  * Created by Al on 18/12/2017.
  */
class SQLTests extends FullSuite[SQLError] {
  override val backend: DBBackend[SQLError] = SQLDB
  implicit override val ec: ExecutionContext = Implicits.global

  override implicit def R: HasRecovery[SQLError] = SQLRecovery
}