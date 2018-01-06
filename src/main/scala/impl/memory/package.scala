package impl

import core.backend.common._
import core.user.containers.ConstrainedFuture
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
  * An in-memory database executor
  */
package object memory extends {
  type MemoryTree = Map[TableName, MemoryTable]
  type RelatedPair = (MemoryObject, MemoryObject)

  implicit class MemoryTreeOps(memoryTree: MemoryTree) {
    def findPattern(findable: UnsafeFindable): MemoryEither[Vector[MemoryObject]] = for {
      table <- memoryTree.getOrError(findable.tableName, MemoryMissingTableName(findable.tableName))
      res <- table.find(findable)
    } yield res

    def findObj(tableName: TableName, obj: DBObject): MemoryEither[Option[MemoryObject]] = for {
      table <- memoryTree.getOrError(tableName, MemoryMissingTableName(tableName))
    } yield table.find(obj)
  }

  type MemoryFuture[A] = ConstrainedFuture[MemoryError, A]
  type MemoryEither[A] = MemoryError \/ A

  def MemoryFutureE[A](ea: MemoryEither[A])(implicit ec: ExecutionContext): MemoryFuture[A] = ConstrainedFuture.either(ea)(errors.recoverMemoryException)
  def MemoryFuture[A](a: => A)(implicit ec: ExecutionContext): MemoryFuture[A] =  ConstrainedFuture.point(a)(errors.recoverMemoryException)
  def MemoryFutureI[A](a: => A)(implicit ec: ExecutionContext): MemoryFuture[A] = ConstrainedFuture.future(Promise.successful(a.right).future)(errors.recoverMemoryException)
  def MemoryFutureER[A](ea: MemoryEither[A])(implicit ec: ExecutionContext): MemoryFuture[A] = ConstrainedFuture.future(Promise.successful(ea).future)(errors.recoverMemoryException)
  def MemoryEither[A](a: => A): MemoryEither[A] = try { a.right} catch {case e: Throwable => errors.recoverMemoryException(e).left}
  def SafeEither[A](ea: => MemoryEither[A]): MemoryEither[A] = try {ea} catch {case e: Throwable => errors.recoverMemoryException(e).left}

  def asE(s: MemoryError): E = s
  def asEither[A](sQLEither: MemoryEither[A]): E \/ A = sQLEither.leftMap(asE)
  def asCFuture[A](f: MemoryFuture[A]): ConstrainedFuture[E, A] = f.leftMap(asE)

  implicit class MemoryFutureOps[A](u: MemoryFuture[A]) {
    def asCFuture: ConstrainedFuture[E, A] = memory.asCFuture(u)
  }

  implicit class MemoryEitherOps[A](u: MemoryEither[A]) {
    def asEither: E \/ A = memory.asEither(u)
  }
}
