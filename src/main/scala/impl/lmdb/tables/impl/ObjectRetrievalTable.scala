package impl.lmdb.tables.impl

import java.nio.ByteBuffer

import core.backend.common.DBObject
import core.backend.intermediate.unsafe.ErasedFindable
import core.user.schema.SchemaObject
import core.utils._
import impl.lmdb
import impl.lmdb.access.Key._
import impl.lmdb.access.{Commit, Key, ObjId}
import impl.lmdb.errors.UnmarshallingError
import impl.lmdb.tables.interfaces.LMDBTable
import impl.lmdb.{LMDBEither, LMDBInstance}
import org.lmdbjava.Dbi
import org.lmdbjava.DbiFlags._

import scalaz.Scalaz._

/**
  * Created by Al on 28/12/2017.
  *
  * A table for the lookup of objectId -> A, used to lookup values
  */
class ObjectRetrievalTable(sa: SchemaObject[_])(implicit val instance: LMDBInstance) extends LMDBTable {
  override val name: String = s"db:objects:${sa.name}"
  override val db: Dbi[ByteBuffer] = instance.env.openDbi(name, MDB_CREATE)

  /**
    * Index tables for this type
    */
  private val indices: Vector[ColumnIndexTable] = sa.schemaComponents.zipWithIndex.map{ // index table for each column
    case (component, column) => new ColumnIndexTable(sa.name, column, component)
  }

  /**
    * Owned empty index table (ie a map of Commit -> set[ObjId]
    */
  private val emptyIndex = new EmptyIndexTable(sa.name)

  /**
    * Find all objects in the commit that match the findable
    */
  def lookup(f: ErasedFindable, commits: Set[Commit]): LMDBEither[Set[ObjId]] =
    for {
      ifEmpty <- emptyIndex.lookupSet(commits)
      indexResults <- EitherOps.sequence(f.pattern.zipWithIndex.collect {case (Some(v), i) => indices(i).lookup(v, commits)})
    } yield bigIntersection(ifEmpty, indexResults)


  /**
    * Find all objects in the collection of commits
    */
  def lookup(commits: Set[Commit]): LMDBEither[Set[ObjId]] = emptyIndex.lookupSet(commits)

  /**
    * Find all objects in the collection of commits
    */
  def lookupVector(commits: Set[Commit]): LMDBEither[Vector[ObjId]] = emptyIndex.lookupVector(commits)

  /**
    * Given an ObjIds, retrieve each ObjId
    */
  def retrieve[A](a: ObjId)(implicit sa: SchemaObject[A]): LMDBEither[A] =
    for {
      dbObject <- get[DBObject](getKey(a))
      a <- sa.fromRow(dbObject).leftMap(UnmarshallingError)
    } yield a

  /**
    * Given a Set of ObjIds, retrieve each ObjId
    */
  def retrieve[A](as: Set[ObjId])(implicit sa: SchemaObject[A]): LMDBEither[Set[A]] =
    EitherOps.sequence(as.map(a => retrieve(a)(sa)))

  /**
    * Given a Vector of ObjIds, retrieve each ObjId
    */
  def retrieve[A](as: Vector[ObjId])(implicit sa: SchemaObject[A]): LMDBEither[Vector[A]] =
    EitherOps.sequence(as.map(retrieve[A]))

  /**
    * Get or create a collection of values.
    * For each object in the collection, get its ObjId or insert it and return the resulting ObjId
    */
  def getOrCreate[A](toCreate: Set[A], commits: Set[Commit], newCommit: Commit)(implicit sa: SchemaObject[A]): LMDBEither[Map[A, ObjId]] =
    EitherOps.sequence(toCreate.map(a => getOrCreate(a, commits, newCommit).map(a -> _))).toMapE


  /**
    * Returns an object id for an A, inserting the A if necessary
    * @return
    */
  // todo: This can be done in bulk?
  private def getOrCreate[A](a: A, commits: Set[Commit], newCommit: Commit)(implicit sa: SchemaObject[A]): LMDBEither[ObjId] =
    for {
      lookupResult <- lookup(sa.findable(a).getUnsafe, commits + newCommit)
      res <- lookupResult.headOption.fold(insert(a, newCommit)){
        _.right
      }
      _ = println("Done get or create: " + a)
    } yield res

  /**
    * Inserts an individual object to a commit
    */
  private def insert[A](a: A, commit: Commit)(implicit sa: SchemaObject[A]): LMDBEither[ObjId] = {
    val insertable = sa.getDBObject(a)
    for {
      // get an objId
      objId <- instance.controlTables.objectCounter.getAndUpdate()

      // insert the object itself
      _ <- this.insertFields(insertable, objId)

      // insert into the empty index
      _ <- emptyIndex.insert(commit, objId)
      // insert into each index table
      _ <- EitherOps.sequence(
        indices.zip(insertable.fields).map {
          // todo: Can we group objects with matching fields?
          case (table, field) => table.insert(field, commit, objId)
        }
      )
    } yield objId
  }

  /**
    * Convert and ObjId into a key
    */
  private def getKey(objId: ObjId): Key = objId.key

  /**
    * Inserts the fields of an object
    */
  private def insertFields(fields: DBObject, objId: ObjId): LMDBEither[Unit] =
    put(getKey(objId), fields)

  /**
    * Nothing to do to initialise
    * @return
    */
  override def initialise(): LMDBEither[Unit] = LMDBEither(())
}
