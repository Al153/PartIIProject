package impl.memory.methods

import core.utils.algorithms
import core.backend.intermediate.unsafe.ErasedFindable
import core.utils._
import impl.memory.errors.{MemoryError, MemoryMissingTableName}
import impl.memory.{MemoryEither, MemoryObject, MemoryPath, MemoryTree, RelatedPair}

/**
  * Implementation of path finding methods
  */
trait PathFinding { self: ExecutorMethods =>

  /**
    * Run the generic allShortestPaths Function
    */
  def allShortestPathsImpl(start: Set[MemoryObject], searchStep: MemoryObject => MemoryEither[Set[MemoryObject]]):  MemoryEither[Set[MemoryPath]] =
    algorithms.PathFinding.allShortestPathsImpl(start, searchStep).map(_.map(MemoryPath.apply))

  /**
    * Run the generic singleShortestPath Function
    */
  def singleShortestsPathImpl(start: Set[MemoryObject], end: ErasedFindable, searchStep: MemoryObject => MemoryEither[Set[MemoryObject]], tree: MemoryTree): MemoryEither[Option[MemoryPath]] =
    for {
      // get the possible end result
      possibleEnds <- tree.getOrError(end.tableName, MemoryMissingTableName(end.tableName)).flatMap(_.find(end).map(_.toSet))
      e = possibleEnds.headOption // see if there is an available end
      res <- e.fold(MemoryEither[Option[MemoryPath]](None)){
        e =>
          algorithms
            .PathFinding
            .singleShortestsPathImpl[MemoryError, MemoryObject](start, e, searchStep)
            .map(_.map(MemoryPath.apply))
      }
    } yield res
}