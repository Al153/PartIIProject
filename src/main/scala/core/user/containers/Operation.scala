package core.user.containers

import core.user.dsl.View

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
  implicit def OperationInterface[E](implicit ec: ExecutionContext, recover: Throwable => E) = new Monad[({ type λ[A] = Operation[E, A] })#λ] {
    def point[A](a: => A): Operation[E, A] = new Operation[E, A] (v => ConstrainedFuture.future(Promise[E \/ (A, View)].success((a, v).right).future)(recover))
    def bind[A, B](fa: Operation[E, A])(f: (A) => Operation[E, B]): Operation[E, B] = fa.flatMap(f)
  }

  /**
    * Standard point method
    */
  def point[E, A](a: A, recover: Throwable => E)(implicit ec: ExecutionContext): Operation[E, A] = new Operation[E, A] (v => ConstrainedFuture.future(Promise[E \/ (A, View)].success((a, v).right).future)(recover))

}
