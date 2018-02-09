package impl.lmdb.logjoins.retrievers

import core.backend.intermediate.unsafe._
import impl.lmdb.common.access.Commit
import impl.lmdb.common.interfaces.LMDBInstance

/**
  * Created by Al on 06/02/2018.
  */
class PatternRetriever(pattern: ErasedFindable, commits: List[Commit])(implicit instance: LMDBInstance) extends UncachedSingleRetriever(
  instance.lookupPattern(pattern, commits)
)
