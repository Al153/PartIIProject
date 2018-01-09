package impl.memory.methods

import core.backend.intermediate.unsafe.ErasedFindable
import impl.memory.MemoryObject

trait ExecutorMethods  {
  def matches(o: MemoryObject, p: ErasedFindable): Boolean = p.matches(o.value) // check that the object matches the pattern
}