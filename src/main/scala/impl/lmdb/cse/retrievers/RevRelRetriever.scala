package impl.lmdb.cse.retrievers

import core.backend.intermediate.unsafe._
import core.utils._
import impl.lmdb.common.access.Commit
import impl.lmdb.common.interfaces.LMDBInstance
/**
  * Created by Al on 06/02/2018.
  */
class RevRelRetriever(r: ErasedRelationAttributes, commits: List[Commit])(implicit instance: LMDBInstance) extends UncachedRelationRetriever(
  objIds =>
    objIds.flatMapE {
      leftObject => for {
        related <- instance.controlTables.reverseRelations.followRelation(leftObject, commits, r.name)
        _ = println("Reverse rel left object = " + leftObject)
        _ = println("Reverse rel = " + related)
      } yield related.map((leftObject, _))
    },
  objId => instance.controlTables.reverseRelations.followRelation(objId, commits, r.name)
)
