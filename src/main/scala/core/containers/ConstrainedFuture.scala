package core.containers

import view.View

import scala.concurrent.{ExecutionContext, Future, Promise}
import scalaz._
import Scalaz._

/**
  * Created by Al on 14/10/2017.
  */
class ConstrainedFuture[E, A] private (private val underlying: EitherT[Future, E, A])(implicit ec: ExecutionContext) {
  def map[B](f: A => B): ConstrainedFuture[E, B] = new ConstrainedFuture(underlying.map(f))
  def flatMap[B](f: A => ConstrainedFuture[E, B]) = new ConstrainedFuture(underlying.flatMap(a => f(a).underlying))
  def run: Future[E \/ A] = underlying.run
  def leftMap[E1](f: E => E1) = new ConstrainedFuture(underlying.leftMap(f))
  def recover[E1](f: E => ConstrainedFuture[E1, A]) = new ConstrainedFuture(
    EitherT(
      underlying.run.flatMap(
        ea => ea.fold(e => f(e).run, a => Promise.successful(a.right).future)
      )
    )
  )

  def andThen(pf: PartialFunction[E \/ A, Unit]): ConstrainedFuture[E, A] = new ConstrainedFuture(EitherT(underlying.run.andThen{case scala.util.Success(ea) => pf(ea)}))
}

object ConstrainedFuture {
    def point[E, A](a: => A)(recover: Throwable => E)(implicit ec: ExecutionContext): ConstrainedFuture[E, A] = new ConstrainedFuture(
      EitherT(
        Future {
          try {
            a.right
          } catch {
            case e: Throwable => recover(e).left
          }
        }
      )
    )

  def either[E, A](ea: => E \/ A)(recover: Throwable => E)(implicit ec: ExecutionContext): ConstrainedFuture[E, A] = new ConstrainedFuture(
    EitherT(
      Future {
        try {ea} catch {case e: Throwable => recover(e).left}
      }
    )
  )

  def future[E, A](fea: Future[E \/ A])(recover: Throwable => E)(implicit ec: ExecutionContext): ConstrainedFuture[E, A] = new ConstrainedFuture(
    EitherT(
      fea.recover {
        case e: Throwable =>
          \/.left[E, A](recover(e))
      }
    )
  )

  implicit class ConstrainedFutureViewSyntax[E, A](underlying: ConstrainedFuture[E, (A, View)]) {
    def proj: ConstrainedFuture[E, A] = underlying.map(_._1)
  }
}
