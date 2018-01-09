package impl.memory.methods

import core.backend.common.{MissingTableName, algorithms}
import core.backend.intermediate.unsafe.ErasedFindable
import core.utils._
import impl.memory.errors.{MemoryError, MemoryMissingTableName}
import impl.memory.{MemoryEither, MemoryObject, MemoryPath, MemoryTree, RelatedPair}

trait PathFinding { self: ExecutorMethods =>
  // Breadth first == dijkstra's

  def allShortestPathsImpl(start: Set[MemoryObject], searchStep: MemoryObject => MemoryEither[Set[RelatedPair]]):  MemoryEither[Set[MemoryPath]] =
    algorithms.PathFinding.allShortestPathsImpl(start, searchStep).map(_.map(MemoryPath.apply))

  def singleShortestsPathImpl(start: Set[MemoryObject], end: ErasedFindable, searchStep: MemoryObject => MemoryEither[Set[RelatedPair]], tree: MemoryTree): MemoryEither[Option[MemoryPath]] =
    for {
      possibleEnds <- tree.getOrError(end.tableName, MemoryMissingTableName(end.tableName)).flatMap(_.find(end).map(_.toSet))
      e = possibleEnds.headOption // see if there is an available end
      res <- e.fold(MemoryEither[Option[MemoryPath]](None)){
        e => algorithms.PathFinding.singleShortestsPathImpl[MemoryError, MemoryObject](start, e, searchStep).map(_.map(MemoryPath.apply))
      }
    } yield res
}