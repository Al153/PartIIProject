package impl

import core.backend.common._
import core.user.containers.{ConstrainedFuture, Operation}
import core.user.dsl.E
import core.backend.intermediate.unsafe._
import core.user.schema.TableName
import core.utils._
import impl.memory.errors.{MemoryError, MemoryMissingTableName}

import scala.concurrent.{ExecutionContext, Promise}
import scalaz._
import Scalaz._

/**
  * Created by Al on 20/10/2017.
  *
  * An in-memory database executor.
  * Stores a separate database per view
  */
package object memory extends {
  /**
    * Convenience Types
    */
  type MemoryTree = Map[TableName, MemoryTable]
  /**
    * Convenience Type
    */
  type RelatedPair = (MemoryObject, MemoryObject)

  /**
    * Convenience methods for tree
    */
  implicit class MemoryTreeOps(memoryTree: MemoryTree) {
    def findPattern(findable: ErasedFindable): MemoryEither[Set[MemoryObject]] = for {
      table <- memoryTree.getOrError(findable.tableName, MemoryMissingTableName(findable.tableName))
      res <- table.find(findable)
    } yield res

    def findObj(tableName: TableName, obj: DBObject): MemoryEither[Option[MemoryObject]] = for {
      table <- memoryTree.getOrError(tableName, MemoryMissingTableName(tableName))
    } yield table.find(obj)
  }

  type MemoryFuture[A] = ConstrainedFuture[MemoryError, A]
  type MemoryEither[A] = MemoryError \/ A
  type MemoryOperation[A] = Operation[MemoryError, A]

  def MemoryFutureE[A](ea: MemoryEither[A])(implicit ec: ExecutionContext): MemoryFuture[A] = ConstrainedFuture.either(ea)
  def MemoryFuture[A](a: => A)(implicit ec: ExecutionContext): MemoryFuture[A] =  ConstrainedFuture.point(a)
  def MemoryFutureI[A](a: => A)(implicit ec: ExecutionContext): MemoryFuture[A] = ConstrainedFuture.future(Promise.successful(a.right).future)
  def MemoryFutureER[A](ea: MemoryEither[A])(implicit ec: ExecutionContext): MemoryFuture[A] = ConstrainedFuture.future(Promise.successful(ea).future)
  def MemoryEither[A](a: => A): MemoryEither[A] = try { a.right} catch {case e: Throwable => errors.MemoryRecovery.recover(e).left}
  def SafeEither[A](ea: => MemoryEither[A]): MemoryEither[A] = try {ea} catch {case e: Throwable => errors.MemoryRecovery.recover(e).left}

  def asE(s: MemoryError): E = s
  def asEither[A](sQLEither: MemoryEither[A]): E \/ A = sQLEither.leftMap(asE)
  def asCFuture[A](f: MemoryFuture[A]): ConstrainedFuture[E, A] = f.leftMap(asE)

}
