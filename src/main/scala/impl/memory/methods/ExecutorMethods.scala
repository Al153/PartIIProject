package impl.memory.methods

import core.backend.intermediate.unsafe.ErasedFindable
import impl.memory.MemoryObject

/**
  * General methods used by [[impl.memory.InMemoryExecutor]]
  */
trait ExecutorMethods  {
  /**
    * Test whether an object matches the findable
    */
  def matches(o: MemoryObject, p: ErasedFindable): Boolean = p.matches(o.value) // check that the object matches the pattern
}