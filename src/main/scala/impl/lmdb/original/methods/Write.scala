package impl.lmdb.original.methods

import core.backend.intermediate.RelationName
import core.user.containers.{Operation, WriteOperation}
import core.user.dsl.{CompletedRelation, E, View}
import core.user.schema.SchemaObject
import core.utils.{EitherOps, _}
import impl.lmdb.common._
import access._
import impl.lmdb.common.errors.{LMDBError, LMDBMissingInsert, LMDBMissingRelation, LMDBMissingTable}

/**
  * Created by Al on 29/12/2017.
  *
  * Implementation of write methods
  */


trait Write { self: Methods =>
  import instance.executionContext

  /**
    * High-level insert method
    * @param t - values to insert
    */
  def insert[A, B](
                    t: TraversableOnce[CompletedRelation[A, B]]
                  )(
    implicit sa: SchemaObject[A], sb: SchemaObject[B]
  ): LMDBOperation[Unit] = new WriteOperation(
    v => LMDBFutureE(doInsert(t, v))
  )

  /**
    * Runs an insert on a view to produce a view in real time (eg this thread/future)
    * @param t - relations to insert
    * @param v - view to insert to
    * @return
    */
  private def doInsert[A, B](t: TraversableOnce[CompletedRelation[A, B]], v: View)(
    implicit sa: SchemaObject[A], sb: SchemaObject[B]
  ): LMDBEither[View] = for {
    // Check the view is valid
    _ <- instance.controlTables.availableViews.validateView(v)
    _ = logger.trace("Checked View")
    // - get a new commit and view id
    commit <- instance.controlTables.commitsCounter.getAndUpdate()

    _ = logger.trace("got commit")
    newView <- instance.controlTables.viewsCounter.getAndUpdate()

    _ = logger.trace("Got View")

    // - insert new set of commits and view as an entry to the views table
    commits <- instance.controlTables.viewsTable.lookupCommits(v)
    _ <- instance.controlTables.viewsTable.newChildView(newView, commit :: commits)

    _ = logger.trace("Set new commits")

    // get new object Ids for find existing ones
    spec <- getObjIds(t, commits, commit)
    (relationsToInsert, reverseRelationsToInsert) = spec

    _ = logger.trace("Got spec")

    // insert forwards relations
    _ <- EitherOps.sequence {
      relationsToInsert.flatMap {
        case (from, to) =>
          to.map {
            case (relationName, destinations) =>
              instance.controlTables.relations.insert(from, commit, relationName, destinations)
          }
      }
    }

    _ = logger.trace("Done forwards relations")

    // insert reverse relations
    _ <- EitherOps.sequence {
      reverseRelationsToInsert.flatMap {
        case (from, to) =>
          to.map {
            case (relationName, destinations) =>
              instance.controlTables.reverseRelations.insert(from, commit, relationName, destinations)
          }
      }
    }

    _ = logger.trace("Done backwards relations")

    // finally, insert the new view into the available views table
    _ <- instance.controlTables.availableViews.insertNewView(newView)

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
                       commits: List[Commit],
                       newCommit: Commit
                     )(implicit sa: SchemaObject[A], sb: SchemaObject[B]
  ): LMDBEither[(Map[ObjId, Map[RelationName, Set[ObjId]]], Map[ObjId, Map[RelationName, Set[ObjId]]])] =
    for {
      // get hold of the tables to insert into
      aLookupTable <- instance.objects.getOrError(sa.name, LMDBMissingTable(sa.name))
      bLookupTable <- instance.objects.getOrError(sb.name, LMDBMissingTable(sb.name))

      // build an index of A -> (RelationName -> Set[B])

      aMap <- t.toSeq.foldLeft(LMDBEither(Map[A, Map[RelationName, Set[B]]]())) {
        case (em, CompletedRelation(a, r, b)) =>
          for {
            m <- em
            er <- instance.schema.getRelation(r).leftMap(LMDBMissingRelation)
            rname = er.name
            ma: Map[RelationName, Set[B]] = m.getOrElse(a, Map[RelationName, Set[B]]())
            mset = ma + (rname -> (ma.getOrElse(rname, Set[B]()) + b))
          } yield m + (a -> mset)
      }

      _ = logger.trace("Built A index")

      // build equivalent B index
      bMap <- t.toSeq.foldLeft(LMDBEither(Map[B, Map[RelationName, Set[A]]]())) {
        case (em, CompletedRelation(a, r, b)) =>
          for {
            m <- em
            er <- instance.schema.getRelation(r).leftMap(LMDBMissingRelation)
            rname = er.name
            mb: Map[RelationName, Set[A]] = m.getOrElse(b, Map[RelationName, Set[A]]())
            mset = mb + (rname -> (mb.getOrElse(rname, Set[A]()) + a))
          } yield m + (b -> mset)
      }


      _ = logger.trace("Built B index")

      // find a lookup table of (ObjId -> A) (ObjId -> B)
      aLookup <- aLookupTable.getOrCreate(aMap.keySet, newCommit :: commits, newCommit)
      bLookup <- bLookupTable.getOrCreate(bMap.keySet, newCommit :: commits, newCommit)

      _ = logger.trace("Got and created all")

      // convert the two lookup maps
      aRes <- convertMap[A, B](aMap, aLookup, bLookup)
      bRes <- convertMap[B, A](bMap, bLookup, aLookup)
    } yield aRes -> bRes

  /**
    * Given a specification of which objects are linked to which, and a pair of lookup tables
    * convert to a specification of [[ObjId]] s
    * @param aMap - the map to convert
    * @param aLookup - A type lookup
    * @param bLookup - B type lookup
    * @return
    */
  private def convertMap[A, B](
                                aMap: Map[A, Map[RelationName, Set[B]]],
                                aLookup: Map[A, ObjId],
                                bLookup: Map[B, ObjId]
  ): LMDBEither[Map[ObjId, Map[RelationName, Set[ObjId]]]] =
    EitherOps.sequence(aMap.map {
      case (a, m) =>
        for {
          aId <- aLookup.getOrError(a, LMDBMissingInsert(a, aLookup): LMDBError)
          map <- EitherOps.sequence(m.map {
            case (r, bs) => EitherOps.sequence(bs.map {
              b => bLookup.getOrError(b, LMDBMissingInsert(b, bLookup): LMDBError)
            }).map(r -> _)
          }).toMapE
        } yield aId -> map}).toMapE

}

