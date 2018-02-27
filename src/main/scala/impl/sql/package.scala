package impl

import core.user.containers.{ConstrainedFuture, Operation}
import core.user.dsl.E
import impl.sql.errors.SQLError

import scalaz._
import Scalaz._
import scala.concurrent.{ExecutionContext, Promise}

/**
  * Created by Al on 18/12/2017.
  *
  * Implementation of [[core.user.interfaces.DBBackend]] using an underlying PostgreSQL database
  */
package object sql {
  /**
    * Convenience types
    */
  type SQLFuture[A] = ConstrainedFuture[SQLError, A]
  type SQLOperation[A] = Operation[SQLError, A]
  type SQLEither[A] = SQLError \/ A

  /**
    * Convenience constructors for SQLFuture and SQLEither
    */
  def SQLFutureE[A](ea: SQLEither[A])(implicit ec: ExecutionContext): SQLFuture[A] = ConstrainedFuture.either(ea)
  def SQLFuture[A](a: => A)(implicit ec: ExecutionContext): SQLFuture[A] =  ConstrainedFuture.point(a)

  /**
    * Immediate constructors (do not delegate to another thread
    */
  def SQLFutureI[A](a: => A)(implicit ec: ExecutionContext): SQLFuture[A] = ConstrainedFuture.future(Promise.successful(a.right).future)
  def SQLFutureER[A](ea: SQLEither[A])(implicit ec: ExecutionContext): SQLFuture[A] = ConstrainedFuture.future(Promise.successful(ea).future)

  /**
    *  Either constructors (safe)
    *
    */
  def SQLEither[A](a: => A): SQLEither[A] = try { a.right} catch {case e: Throwable => errors.SQLRecovery.recover(e).left}
  def SafeEither[A](ea: => SQLEither[A]): SQLEither[A] = try {ea} catch {case e: Throwable => errors.SQLRecovery.recover(e).left}

  /**
    * Conversions to regular constrained futures and eithers
    */
  def asE(s: SQLError): E = s
  def asEither[A](sQLEither: SQLEither[A]): E \/ A = sQLEither.leftMap(asE)
  def asCFuture[A](f: SQLFuture[A]): ConstrainedFuture[E, A] = f.leftMap(asE)
}
