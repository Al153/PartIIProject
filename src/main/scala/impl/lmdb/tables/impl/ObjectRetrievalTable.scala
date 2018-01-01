package impl.lmdb.tables.impl

import core.backend.common.DBObject
import core.intermediate.unsafe.{SchemaObjectErased, UnsafeFindable}
import core.schema.SchemaObject
import core.utils._
import impl.lmdb.access.Key._
import impl.lmdb.access.{Commit, Key, ObjId}
import impl.lmdb.errors.LMDBMissingTable
import impl.lmdb.tables.interfaces.LMDBTable
import impl.lmdb.{BigSetOps, LMDBEither, LMDBInstance}

import scalaz.Scalaz._
import scalaz._

/**
  * Created by Al on 28/12/2017.
  *
  * A table for the lookup of objectId -> A
  */
class ObjectRetrievalTable(sa: SchemaObjectErased)(implicit val instance: LMDBInstance) extends LMDBTable {
  override val path: Key = "db".key :: "objects".key :: sa.name.key
  val indices: Vector[ColumnIndexTable] = sa.schemaComponents.zipWithIndex.map{ // index table for each column
    case (component, column) => new ColumnIndexTable(sa.name, column, component)
  }
  val emptyIndex = new EmptyIndexTable(sa.name)

  def lookup(f: UnsafeFindable, commits: Set[Commit]): LMDBEither[Set[ObjId]] = {
    for {
      ifEmpty <- emptyIndex.lookupSet(commits)
      indexResults <- EitherOps.sequence(f.pattern.zipWithIndex.collect {case (Some(v), i) => indices(i).lookup(v, commits)})
    } yield BigSetOps.bigIntersection(ifEmpty, indexResults)
  }

  def lookup(commits: Set[Commit]): LMDBEither[Set[ObjId]] = emptyIndex.lookupSet(commits)
  def lookupVector(commits: Set[Commit]): LMDBEither[Vector[ObjId]] = emptyIndex.lookupVector(commits)

  def retrieve[A](a: ObjId)(implicit sa: SchemaObject[A]): LMDBEither[A] = ???
  def retrieve[A](as: Set[ObjId])(implicit sa: SchemaObject[A]): LMDBEither[Set[A]] = ???
  def retrieve[A](as: Vector[ObjId])(implicit sa: SchemaObject[A]): LMDBEither[Vector[A]] = ???

  def lookupPattern(p: UnsafeFindable, commits: Set[Commit]): LMDBEither[Set[ObjId]] =
    if (p.tableName in instance.objects) instance.objects(p.tableName).lookup(p, commits)
    else LMDBMissingTable(p.tableName).left

  def getOrCreate[A](toCreate: Set[A], commits: Set[Commit], newCommit: Commit)(implicit sa: SchemaObject[A]): LMDBEither[Map[A, ObjId]] =
    EitherOps.sequence(toCreate.map(a => getOrCreate(a, commits, newCommit).map(a -> _))).toMapE


  // todo: This can be done in bulk?
  private def getOrCreate[A](a: A, commits: Set[Commit], newCommit: Commit)(implicit sa: SchemaObject[A]): LMDBEither[ObjId] =
    for {
      lookupResult <- lookup(sa.findable(a).getUnsafe, commits + newCommit)
      res <- lookupResult.find(_ => true).fold(insert(a, newCommit)){
        _.right
      }
    } yield res


  private def insert[A](a: A, commit: Commit)(implicit sa: SchemaObject[A]): LMDBEither[ObjId] = {
    val insertable = sa.getDBObject(a)
    for {
    // Insert to retrieval table
      objId <- instance.controlTables.objectCounter.getAndUpdate()
      _ <- emptyIndex.insert(commit, objId)

      _ <- EitherOps.sequence(
        indices.zip(insertable.fields).map {
          case (table, field) => table.insert(field, commit, objId)
        }
      )

      _ <- this.insertFields(insertable, commit, objId)


    // insert to each index table
    } yield objId
  }

  private def insertFields(fields: DBObject, commit: Commit, objId: ObjId): LMDBEither[Unit] = ???
}
