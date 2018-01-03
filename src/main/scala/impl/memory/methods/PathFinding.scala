package impl.memory.methods

import core.backend.common.{EmptyFringeError, MissingTableName, algorithms}
import core.error.E
import core.intermediate.unsafe.UnsafeFindable
import core.utils._
import impl.memory.{MemoryObject, MemoryPath, MemoryTree, RelatedPair}

import scalaz.Scalaz._
import scalaz._

trait PathFinding { self: ExecutorMethods =>
  // Breadth first == dijkstra's

  def allShortestPathsImpl(start: Set[MemoryObject], searchStep: MemoryObject => E \/ Set[RelatedPair]): E \/ Set[MemoryPath] =
    algorithms.PathFinding.allShortestPathsImpl[E, MemoryObject](start, searchStep, EmptyFringeError).map(_.map(MemoryPath.apply))

  def singleShortestsPathImpl(start: Set[MemoryObject], end: UnsafeFindable, searchStep: MemoryObject => E \/ Set[RelatedPair], tree: MemoryTree): E \/ Option[MemoryPath] =
    for {
      possibleEnds <- tree.getOrError(end.tableName, MissingTableName(end.tableName)).flatMap(_.find(end).map(_.toSet))
      e = possibleEnds.headOption // see if there is an available end
      res <- e.fold((None:  Option[MemoryPath]).right[E]){
        e => algorithms.PathFinding.singleShortestsPathImpl[E, MemoryObject](start, e, searchStep, EmptyFringeError).map(_.map(MemoryPath.apply))
      }
    } yield res
}