package impl.lmdb.tables.impl

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
      ifEmpty <- emptyIndex.lookup(commits)
      indexResults <- EitherOps.sequence(f.pattern.zipWithIndex.collect {case (Some(v), i) => indices(i).lookup(v, commits)})
    } yield BigSetOps.bigIntersection(ifEmpty, indexResults)
  }

  def lookup(commits: Set[Commit]): LMDBEither[Set[ObjId]] = emptyIndex.lookup(commits)
  def lookupVector(commits: Set[Commit]): LMDBEither[Vector[ObjId]] = emptyIndex.lookupVector(commits)

  def retrieve[A](a: ObjId)(implicit sa: SchemaObject[A]): LMDBEither[A] = ???
  def retrieve[A](as: Set[ObjId])(implicit sa: SchemaObject[A]): LMDBEither[Set[A]] = ???
  def retrieve[A](as: Vector[ObjId])(implicit sa: SchemaObject[A]): LMDBEither[Vector[A]] = ???

  def lookupPattern(p: UnsafeFindable, commits: Set[Commit]): LMDBEither[Set[ObjId]] =
    if (p.tableName in instance.objects) instance.objects(p.tableName).lookup(p, commits)
    else LMDBMissingTable(p.tableName).left

}
