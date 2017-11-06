package impl.memory.methods

import core.intermediate.unsafe.UnsafeFindable
import impl.memory.MemoryObject

trait ExecutorMethods  {
  def matches(o: MemoryObject, p: UnsafeFindable): Boolean = p.matches(o.value) // check that the object matches the pattern
}