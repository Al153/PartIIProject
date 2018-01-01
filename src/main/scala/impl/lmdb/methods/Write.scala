package impl.lmdb.methods

import core.containers.{Operation, WriteOperation}
import core.error.E
import core.relations.CompletedRelation
import core.schema.{RelationName, SchemaDescription, SchemaObject}
import core.utils.EitherOps
import core.view.View
import impl.lmdb._
import impl.lmdb.access.{Commit, ObjId}

/**
  * Created by Al on 29/12/2017.
  */


trait Write { self: Methods =>
  import instance._

  def insert[A, B](
                    t: TraversableOnce[CompletedRelation[A, B]]
                  )(
    implicit sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription
  ): Operation[E, Unit] = new WriteOperation[E](
    v => LMDBFutureE(doInsert(t, v)).asCFuture
  )

  private def doInsert[A, B](t: TraversableOnce[CompletedRelation[A, B]], v: View)(
    implicit sa: SchemaObject[A], sb: SchemaObject[B], sd: SchemaDescription
  ): LMDBEither[View] = for {
    _ <- instance.controlTables.availableViews.validateView(v)
    // - get a new commit and view id
    commit <- instance.controlTables.commitsCounter.getAndUpdate()
    newView <- instance.controlTables.viewsCounter.getAndUpdate()

    // - insert new set of commits and view as an entry to the views table

    commits <- instance.controlTables.viewsTable.lookupCommits(v)
    _ <- instance.controlTables.viewsTable.newChildView(newView, commits + commit)

    spec <- getObjIds(t, commits, commit)
    (relationsToInsert, reverseRelationsToInsert) = spec

    _ <- EitherOps.sequence {
      relationsToInsert.flatMap {
        case (from, to) =>
          to.map {
            case (relationName, destinations) =>
              instance.controlTables.relations.insert(from, commit, relationName, destinations)
          }
      }
    }
    _ <- EitherOps.sequence {
      reverseRelationsToInsert.flatMap {
        case (from, to) =>
          to.map {
            case (relationName, destinations) =>
              instance.controlTables.reverseRelations.insert(from, commit, relationName, destinations)
          }
      }
    }

    _ <- instance.controlTables.availableViews.insertNewView(newView)

    /*
      * Ideas
      *
      *   -
      *   - run an insert or get on all the objects in the relation
      *   - insert all of the relations
      *   - Add to available views
     */
  } yield newView


  /**
    * converts a collection of [[CompletedRelation]] into a specification of relations to insert
    * @param t - collection of relations to insert
    * @param commits - old commits to search
    * @param newCommit - commit to add new commits to
    *
    * @param sa - SchemaObject of A
    * @param sb - SchemaObject of B
    * @tparam A - type of left of relations
    * @tparam B - type of right of relations
    * @return
    */
  private def getObjIds[A, B](
                       t: TraversableOnce[CompletedRelation[A, B]],
                       commits: Set[Commit],
                       newCommit: Commit
                     )(
    implicit sa: SchemaObject[A],
    sb: SchemaObject[B]
  ): LMDBEither[(Map[ObjId, Map[RelationName, Set[ObjId]]], Map[ObjId, Map[RelationName, Set[ObjId]]])] = ???
}

