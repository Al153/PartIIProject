package impl.lmdb.common.tables.interfaces

import core.backend.intermediate.RelationName
import impl.lmdb.common.LMDBEither
import impl.lmdb.common.access.Key._
import impl.lmdb.common.access.{Commit, Key, ObjId}


/**
  * Created by Al on 29/12/2017.
  *
  * A relation table stores all of a certain class of relations (ie forwards, and backwards)
  * from all objects
  */
trait RelationTable extends LMDBTable {

  /**
    * Generates lookup keys from the object, commit, and relation name
    */
  private def getKey(objId: ObjId, commit: Commit, relationName: RelationName): Key =
   objId >> commit >> relationName

  /**
    * Follow a relation from an object
    * @param objId - the id of the start object
    * @param commits - the commits from which we want to extract the relations
    * @param relation - relation name to follow
    * @return a set of objects that are connected by the relation
    */
  def followRelation(objId: ObjId, commits: List[Commit], relation: RelationName): LMDBEither[Set[ObjId]] =
    getBatch[Set[ObjId], List](commits.map(getKey(objId, _, relation))).map(_.toSet.flatten)

  /**
    * Insert a set of objects reachable in a commit
    * @param from - the start object
    * @param commit - the commit to write into
    * @param relationName - name of relation to insert
    * @param to - set of destinations
   */
  def insert(from: ObjId, commit: Commit, relationName: RelationName, to: Set[ObjId]): LMDBEither[Unit] =
      transactionalUnion(getKey(from, commit, relationName), to)

  /**
    * Do nothing to initialise
    * @return
    */
  override def initialise(): LMDBEither[Unit] = LMDBEither(())
}
