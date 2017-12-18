package unit

import core.backend.interfaces.DBBackend
import impl.sql.SQLDB
import unit.suites._

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits

/**
  * Created by Al on 18/12/2017.
  */
class SQLTests extends HasBackend
  with Duplicates
  with IntersectionsAndDisjunctions
  with ReadWrite
  with Transitive
  with Repetition
  with LoopedRepetition
  with ComplexRepetition
  with Pathfinding {
  override val backend: DBBackend = SQLDB
  implicit override val ec: ExecutionContext = Implicits.global
}