package core.user.containers

import core.user.dsl.{HasRecovery, View}

import scala.concurrent.{ExecutionContext, Promise}
import scala.language.reflectiveCalls
import scalaz.Scalaz._
import scalaz._

/**
  * Created by Al on 14/10/2017.
  *
  * An Operation is a monad transformer stack representing a ConstrainedFuture of results of computations wrapped into a state monad
  */

case class Operation[E, A](runView: View => ConstrainedFuture[E, (A, View)])(implicit e: ExecutionContext) {
  /**
    * Standard map method
    */
  def map[B](f: A => B): Operation[E, B] = _map(this, f)
  /**
    * Standard flatMap method
    */
  def flatMap[B](f: A => Operation[E, B]): Operation[E, B] = _flatMap(this, f)
  /**
    * Standard foreach method
    */
  def foreach(f: A => Unit): Operation[E, Unit] = _map(this, f)

  /**
    * Transform the leftmost error
    */
  def leftMap[E1](f: E => E1)(implicit r: HasRecovery[E1]): Operation[E1, A] = _leftMap(this, f)

  private def _leftMap[E1](outer: Operation[E, A], f: E => E1)(implicit r: HasRecovery[E1]): Operation[E1, A] =
    new Operation[E1, A] (
      u => outer.runView(u).leftMap(f)
    )

  private def _map[B](outer: Operation[E, A], f: A => B): Operation[E, B] = new Operation[E, B] (
    u => {
      val av = outer.runView(u)
      av.map {case (a, v) => (f(a), v)}
    }
  )

  private def _flatMap[B](outer: Operation[E, A], f: A => Operation[E, B]) = new Operation[E, B](
    u => {
      val av = outer.runView(u)
      av.flatMap {
        case (a, v) =>
          f(a).runView(v)
      }
    }
  )


}

object Operation {
  /**
   * Scalaz Monad instance for Operations
   */
  implicit def OperationInterface[E](implicit ec: ExecutionContext, recover: Throwable => E, R: HasRecovery[E]) = new Monad[({ type λ[A] = Operation[E, A] })#λ] {
    def point[A](a: => A): Operation[E, A] = new Operation[E, A] (v => ConstrainedFuture.future(Promise[E \/ (A, View)].success((a, v).right).future))
    def bind[A, B](fa: Operation[E, A])(f: (A) => Operation[E, B]): Operation[E, B] = fa.flatMap(f)
  }

  /**
    * Standard point method
    */
  def point[E, A](a: A, recover: Throwable => E)(implicit ec: ExecutionContext, R: HasRecovery[E]): Operation[E, A] = new Operation[E, A] (v => ConstrainedFuture.future(Promise[E \/ (A, View)].success((a, v).right).future))


  implicit class OperationOps[E <: core.user.dsl.E, A](u: Operation[E, A]) {
    def eraseError: Operation[core.user.dsl.E, A] = u.leftMap(e => e: E)
  }
}
