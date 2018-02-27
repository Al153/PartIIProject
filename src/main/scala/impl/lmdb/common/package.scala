package impl.lmdb

import core.user.containers.{ConstrainedFuture, Operation}
import core.user.dsl.{E, ViewId}
import impl.lmdb.common.errors._

import scala.concurrent.{ExecutionContext, Promise}
import scalaz.Scalaz._
import scalaz._

/**
  * Created by Al on 28/12/2017.
  */
package object common {

  /**
    * Convenience types
    */
  type LMDBEither[A] = LMDBError \/ A
  type LMDBFuture[A] = ConstrainedFuture[LMDBError, A]
  type LMDBOperation[A] = Operation[LMDBError, A]


  /**
    * LMDBFuture convenience methods, self explanatory

    */
  def LMDBFutureE[A](ea: LMDBEither[A])(implicit ec: ExecutionContext): LMDBFuture[A] = ConstrainedFuture.either(ea)
  def LMDBFuture[A](a: => A)(implicit ec: ExecutionContext): LMDBFuture[A] =  ConstrainedFuture.point[LMDBError, A](a)

  /**
    * Use a promise to avoid delagating small piece of work to futures
    */
  def LMDBFutureI[A](a: => A)(implicit ec: ExecutionContext): LMDBFuture[A] = ConstrainedFuture.future[LMDBError, A](Promise.successful(a.right).future)
  def LMDBFutureER[A](ea: LMDBEither[A])(implicit ec: ExecutionContext): LMDBFuture[A] = ConstrainedFuture.future(Promise.successful(ea).future)
  def LMDBFailure[A](e: => LMDBError)(implicit ec: ExecutionContext): LMDBFuture[A] = ConstrainedFuture.future[LMDBError, A](Promise.successful(e.left).future)
  def LMDBEither[A](a: => A): LMDBEither[A] = try { a.right} catch {case e: Throwable => LMDBRecover.recover(e).left}
  def safeEither[A](ea: => LMDBEither[A]): LMDBEither[A] = try {ea} catch {case e: Throwable => LMDBRecover.recover(e).left}
  def LMDBLeft[A](e: => LMDBError): LMDBEither[A] = try {e.left} catch {case e: Throwable => LMDBRecover.recover(e).left}

  def asE(s: LMDBError): E = s
  def asEither[A](sQLEither: LMDBEither[A]): E \/ A = sQLEither.leftMap(asE)
  def asCFuture[A](f: LMDBFuture[A]): ConstrainedFuture[E, A] = f.leftMap(asE)


  // Set up the initial view of the DB
  val initialView = ViewId(0)

}
