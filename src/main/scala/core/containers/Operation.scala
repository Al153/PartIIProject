package core.containers

import view.View

import scala.concurrent.{ExecutionContext, Promise}
import scala.language.reflectiveCalls
import scalaz.Scalaz._
import scalaz._

/**
  * Created by Al on 14/10/2017.
  *
  * An extract is a monad transformer stack representing a restricted future of a sequence of results of computations
  */

case class Operation[E, A](runView: View => ConstrainedFuture[E, (A, View)])(implicit e: ExecutionContext) {
  def map[B](f: A => B): Operation[E, B] = _map(this, f)
  def flatMap[B](f: A => Operation[E, B]): Operation[E, B] = _flatMap(this, f)
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
  implicit def extractInterface[E] = new Monad[({ type λ[A] = Operation[E, A] })#λ] {
    def point[A](a: => A): Operation[E, A] = new Operation[E, A] (v => ConstrainedFuture(Promise[E \/ (A, View)].success((a, v).right).future)(???))
    def bind[A, B](fa: Operation[E, A])(f: (A) => Operation[E, B]): Operation[E, B] = fa.flatMap(f)
  }

  def apply[E, A](runView: View => ConstrainedFuture[E, (A, View)]): Operation[E, A] = new Operation(runView)
}
