package impl.lmdb.logjoins.retrievers

import core.backend.intermediate.unsafe._
import core.utils._
import impl.lmdb.common.access.Commit
import impl.lmdb.common.interfaces.LMDBInstance


class RelRetriever(r: ErasedRelationAttributes, commits: List[Commit])(implicit instance: LMDBInstance) extends UncachedRelationRetriever(
  objIds =>
    objIds.flatMapE {
      leftObject => for {
        related <- instance.controlTables.relations.followRelation(leftObject, commits, r.name)
      } yield related.map((leftObject, _))
    },
  objId => instance.controlTables.relations.followRelation(objId, commits, r.name)
)





