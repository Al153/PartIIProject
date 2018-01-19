package impl

import core.user.containers.ConstrainedFuture
import core.user.dsl.{E, View}
import impl.lmdbfast.errors.{LMDBError, _}

import scala.concurrent.{ExecutionContext, Promise}
import scalaz.Scalaz._
import scalaz._

/**
  * Created by Al on 19/01/2018.
  *
  * An optimised version of the LMDB Implementation
  */
package object lmdbfast {

  /**
    * Convenience types
    */
  type LMDBEither[A] = LMDBError \/ A
  type LMDBFuture[A] = ConstrainedFuture[LMDBError, A]


  /**
    * LMDBFuture convenience methods, self explanatory

    */
  def LMDBFutureE[A](ea: LMDBEither[A])(implicit ec: ExecutionContext): LMDBFuture[A] = ConstrainedFuture.either(ea)(recoverLMDBException)
  def LMDBFuture[A](a: => A)(implicit ec: ExecutionContext): LMDBFuture[A] =  ConstrainedFuture.point(a)(recoverLMDBException)

  /**
    * Use a promise to avoid delagating small piece of work to futures
    */
  def LMDBFutureI[A](a: => A)(implicit ec: ExecutionContext): LMDBFuture[A] = ConstrainedFuture.future(Promise.successful(a.right).future)(recoverLMDBException)
  def LMDBFutureER[A](ea: LMDBEither[A])(implicit ec: ExecutionContext): LMDBFuture[A] = ConstrainedFuture.future(Promise.successful(ea).future)(recoverLMDBException)
  def LMDBFailure[A](e: => LMDBError)(implicit ec: ExecutionContext): LMDBFuture[A] = ConstrainedFuture.future(Promise.successful(e.left).future)(recoverLMDBException)
  def LMDBEither[A](a: => A): LMDBEither[A] = try { a.right} catch {case e: Throwable => recoverLMDBException(e).left}
  def safeEither[A](ea: => LMDBEither[A]): LMDBEither[A] = try {ea} catch {case e: Throwable => recoverLMDBException(e).left}
  def LMDBLeft[A](e: => LMDBError): LMDBEither[A] = try {e.left} catch {case e: Throwable => recoverLMDBException(e).left}

  def asE(s: LMDBError): E = s
  def asEither[A](sQLEither: LMDBEither[A]): E \/ A = sQLEither.leftMap(asE)
  def asCFuture[A](f: LMDBFuture[A]): ConstrainedFuture[E, A] = f.leftMap(asE)




  /**
    * Syntax for LMDB Futures
    */
  implicit class LMDBFutureOps[A](u: LMDBFuture[A]) {
    /**
      * Convert and LMDBFuture to an E ConstrainedFuture A
      * @return
      */
    def asCFuture: ConstrainedFuture[E, A] = lmdbfast.asCFuture(u)
  }

  /**
    * Syntax for LMDB Eithers
    */

  implicit class LMDBEitherOps[A](u: LMDBEither[A]) {
    /**
      * Convert and LMDBEither to an E \/ A
      * @return
      */
    def asEither: E \/ A = lmdbfast.asEither(u)
  }



  // Set up the initial view of the DB
  val initialView = View(0)

}
