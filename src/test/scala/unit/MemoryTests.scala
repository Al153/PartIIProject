package unit

import core.backend.interfaces.DBBackend
import impl.memory.MemoryDB
import unit.suites._

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits

class MemoryTests extends HasBackend
  with Duplicates
  with IntersectionsAndDisjunctions
  with ReadWrite
  with Transitive
  with Repetition
  with LoopedRepetition
  with ComplexRepetition
  with SimplePathFinding
  with ComplexPathFinding {
  override val backend: DBBackend = MemoryDB
  implicit override val ec: ExecutionContext = Implicits.global
}