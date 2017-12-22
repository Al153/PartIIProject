package impl

import core.containers.ConstrainedFuture
import core.error.E
import impl.sql.errors.SQLError

import scalaz._
import Scalaz._
import scala.concurrent.{ExecutionContext, Promise}

/**
  * Created by Al on 18/12/2017.
  */
package object sql {
  type SQLFuture[A] = ConstrainedFuture[SQLError, A]
  type SQLEither[A] = SQLError \/ A

  def SQLFutureE[A](ea: SQLEither[A])(implicit ec: ExecutionContext): SQLFuture[A] = ConstrainedFuture.either(ea)(errors.recoverSQLException)
  def SQLFuture[A](a: => A)(implicit ec: ExecutionContext): SQLFuture[A] =  ConstrainedFuture.point(a)(errors.recoverSQLException)
  def SQLFutureI[A](a: => A)(implicit ec: ExecutionContext): SQLFuture[A] = ConstrainedFuture.future(Promise.successful(a.right).future)(errors.recoverSQLException)
  def SQLFutureER[A](ea: SQLEither[A])(implicit ec: ExecutionContext): SQLFuture[A] = ConstrainedFuture.future(Promise.successful(ea).future)(errors.recoverSQLException)
  def SQLEither[A](a: => A): SQLEither[A] = try { a.right} catch {case e: Throwable => errors.recoverSQLException(e).left}
  def SafeEither[A](ea: => SQLEither[A]): SQLEither[A] = try {ea} catch {case e: Throwable => errors.recoverSQLException(e).left}

  def asE(s: SQLError): E = s
  def asEither[A](sQLEither: SQLEither[A]): E \/ A = sQLEither.leftMap(asE)
  def asCFuture[A](f: SQLFuture[A]): ConstrainedFuture[E, A] = f.leftMap(asE)

  implicit class SQLFutureOps[A](u: SQLFuture[A]) {
    def asCFuture: ConstrainedFuture[E, A] = sql.asCFuture(u)
  }

  implicit class SQLEitherOps[A](u: SQLEither[A]) {
    def asEither: E \/ A = sql.asEither(u)
  }
}
