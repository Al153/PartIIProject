package impl

import core.containers.ConstrainedFuture
import impl.sql.errors.SQLError

import scalaz._
import Scalaz._
import scala.concurrent.ExecutionContext

/**
  * Created by Al on 18/12/2017.
  */
package object sql {
  type SQLFuture[A] = ConstrainedFuture[SQLError, A]
  type SQLEither[A] = SQLError \/ A

  def SQLFutureE[A](ea: SQLEither[A])(implicit ec: ExecutionContext): SQLFuture[A] = ConstrainedFuture.either(ea)(errors.recoverSQLException)
  def SQLFuture[A](a: => A)(implicit ec: ExecutionContext): SQLFuture[A] =  ConstrainedFuture.point(a)(errors.recoverSQLException)
  def SQLEither[A](a: => A): SQLEither[A] = try { a.right} catch {case e: Throwable => errors.recoverSQLException(e).left}
  def SafeEither[A](ea: => SQLEither[A]): SQLEither[A] = try {ea} catch {case e: Throwable => errors.recoverSQLException(e).left}
}
