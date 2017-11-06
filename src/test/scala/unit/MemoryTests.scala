package unit

import db.interfaces.DBBackend
import db.memory.MemoryDB
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
  with Pathfinding {
  override val backend: DBBackend = MemoryDB
  implicit override val ec: ExecutionContext = Implicits.global
}