package impl.memory

import core.backend.common.{DBObject, ExtractError}
import core.backend.intermediate.unsafe.ErasedPath
import core.user.containers.Path
import core.user.schema.SchemaObject
import core.utils.EitherOps
import impl.memory.errors.MemoryExtractError

import scalaz.\/

/**
  * Created by Al on 27/10/2017.
  *
  * Memory specific implementation of a Path
  */
case class MemoryPath private (p: List[MemoryObject]) {

  /**
    * Convert to a proper paths
    * @return
    */
  def toPath[A](implicit sa: SchemaObject[A]): MemoryEither[Path[A]] =
    EitherOps
      .sequence(p.map(o => sa.fromRow(o.value)))
      .map(Path.fromList)
      .leftMap(MemoryExtractError)

  /**
    * Add an object to the path
    */
  def +(m: MemoryObject): MemoryPath = MemoryPath(m :: p)

  /**
    * Get the last value from the path
    * @return
    */
  def getLast: MemoryObject = p.last
}

object MemoryPath {
  /**
    * Alternate constructor
    */
  def apply(ps: MemoryObject*): MemoryPath = new MemoryPath(ps.toList)
}