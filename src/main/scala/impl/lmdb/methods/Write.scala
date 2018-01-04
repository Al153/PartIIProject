package impl.lmdb.methods

import core.containers.{Operation, WriteOperation}
import core.error.E
import core.relations.CompletedRelation
import core.schema.{RelationName, SchemaDescription, SchemaObject}
import core.utils.EitherOps
import core.view.View
import impl.lmdb._
import impl.lmdb.access.{Commit, ObjId}
import core.utils._
import impl.lmdb.errors.{LMDBError, LMDBMissingInsert, LMDBMissingRelation, LMDBMissingTable}

import scalaz._
import Scalaz._

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
    sb: SchemaObject[B],
    sd: SchemaDescription
  ): LMDBEither[(Map[ObjId, Map[RelationName, Set[ObjId]]], Map[ObjId, Map[RelationName, Set[ObjId]]])] =
    for {
      aLookupTable <- instance.objects.getOrError(sa.tableName, LMDBMissingTable(sa.tableName))
      bLookupTable <- instance.objects.getOrError(sb.tableName, LMDBMissingTable(sb.tableName))

      aMap <- t.toSeq.foldLeft(LMDBEither(Map[A, Map[RelationName, Set[B]]]())) {
        case (em, CompletedRelation(a, r, b)) =>
          for {
            m <- em
            er <- sd.getRelation(r).leftMap(LMDBMissingRelation)
            rname = er.name
            ma: Map[RelationName, Set[B]] = m.getOrElse(a, Map[RelationName, Set[B]]())
            mset = ma + (rname -> (ma.getOrElse(rname, Set[B]()) + b))
          } yield m + (a -> mset)
      }

      bMap <- t.toSeq.foldLeft(LMDBEither(Map[B, Map[RelationName, Set[A]]]())) {
        case (em, CompletedRelation(a, r, b)) =>
          for {
            m <- em
            er <- sd.getRelation(r).leftMap(LMDBMissingRelation)
            rname = er.name
            mb: Map[RelationName, Set[A]] = m.getOrElse(b, Map[RelationName, Set[A]]())
            mset = mb + (rname -> (mb.getOrElse(rname, Set[A]()) + a))
          } yield m + (b -> mset)
      }


      aLookup <- aLookupTable.getOrCreate(aMap.keySet, commits, newCommit)
      bLookup <- bLookupTable.getOrCreate(bMap.keySet, commits, newCommit)

      aRes <- convertMap[A, B](aMap, aLookup, bLookup)
      bRes <- convertMap[B, A](bMap, bLookup, aLookup)
    } yield aRes -> bRes

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

