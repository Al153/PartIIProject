package impl.lmdb.tables.impl

import core.intermediate.unsafe.{SchemaObjectErased, UnsafeFindable}
import core.schema.SchemaObject
import impl.lmdb.LMDBInstance
import impl.lmdb.access.Key._
import impl.lmdb.access.{Commit, Key, ObjId}
import impl.lmdb.containers.{Extractor, SingleExtractor}
import impl.lmdb.tables.interfaces.LMDBTable

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

  def lookup(f: UnsafeFindable, commit: Commit): Extractor[ObjId] = {
    Extractor
      .bigIntersection(emptyIndex.lookup(commit))(
        f.pattern.zipWithIndex.collect {case (Some(v), i) => indices(i).lookup(v, commit)}
      )
  }

  def lookup(commit: Commit): Extractor[ObjId] = emptyIndex.lookup(commit)

  def retrieve[A](a: ObjId)(implicit sa: SchemaObject[A]): SingleExtractor[A] = ???
  def retrieve[A](as: Iterable[ObjId])(implicit sa: SchemaObject[A]): Extractor[A] = ???



}
