package impl.lmdb.tables.interfaces

import core.utils.EitherOps
import impl.lmdb.LMDBEither
import impl.lmdb.access.{Commit, Key, ObjId}
import impl.lmdb._
import Key._
import core.backend.intermediate.RelationName
import org.fusesource.lmdbjni.Database

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
  def followRelation(objId: ObjId, commits: Set[Commit], relation: RelationName): LMDBEither[Set[ObjId]] =
    EitherOps.sequence(
      commits.map {
        commit =>
          get[Set[ObjId]](db, getKey(objId, commit, relation))
      }
    ).map(_.flatten)

  /**
    * Insert a set of objects reachable in a commit
    * @param from - the start object
    * @param commit - the commit to write into
    * @param relationName - name of relation to insert
    * @param to - set of destinations
   */
  def insert(from: ObjId, commit: Commit, relationName: RelationName, to: Set[ObjId]): LMDBEither[Unit] =
      transactionalUnion(getKey(from, commit, relationName), to, db)

  /**
    * Do nothing to initialise
    * @return
    */
  override def initialise(): LMDBEither[Unit] = LMDBEither(())
}
