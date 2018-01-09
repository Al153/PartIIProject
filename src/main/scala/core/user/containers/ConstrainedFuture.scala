package core.user.containers

import core.user.dsl.View

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.language.higherKinds
import scalaz.Scalaz._
import scalaz._

/**
  * Created by Al on 14/10/2017.
  *
  *  A [[ConstrainedFuture]] with parameters E, A is a Future that can only return a \/-(A) or -\/(E), with all potential
  *  exceptions caught and recovered
  *
  *  This is a monad, so can be chained in a for comprehension
  */
class ConstrainedFuture[E, A] private (private val underlying: EitherT[Future, E, A])(implicit val ec: ExecutionContext) {
  /**
    * Standard map method
    */
  def map[B](f: A => B): ConstrainedFuture[E, B] = new ConstrainedFuture(underlying.map(f))
  /**
    * Standard flatMap method
    */
  def flatMap[B](f: A => ConstrainedFuture[E, B]) = new ConstrainedFuture(underlying.flatMap(a => f(a).underlying))

  /**
    * @return the underlying Future[E \/ A]
    */
  def run: Future[E \/ A] = underlying.run

  /**
    * Map on the left parameter
    */

  def leftMap[E1](f: E => E1) = new ConstrainedFuture(underlying.leftMap(f))

  /**
    * Recover an error: E
    */
  def recover[E1](f: E => ConstrainedFuture[E1, A]) = new ConstrainedFuture(
    EitherT(
      underlying.run.flatMap(
        ea => ea.fold(e => f(e).run, a => Promise.successful(a.right).future)
      )
    )
  )

  /**
    * Chain sideeffecting functions
    */
  def andThen(pf: PartialFunction[E \/ A, Unit]): ConstrainedFuture[E, A] = new ConstrainedFuture(EitherT(underlying.run.andThen{case scala.util.Success(ea) => pf(ea)}))
}

object ConstrainedFuture {
  /**
    * Construct a [[ConstrainedFuture]] simply
   */
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

  /**
    * Construct a [[ConstrainedFuture]] where we know there are no errors
    */
  private def immediatePoint[E, A](a: A)(implicit ec: ExecutionContext): ConstrainedFuture[E, A] =
    new ConstrainedFuture(
      EitherT(
        Promise.successful(a.right[E]).future
      )
    )

  /**
    * Construct a constrained future from an appropriate either
    */
  def either[E, A](ea: => E \/ A)(recover: Throwable => E)(implicit ec: ExecutionContext): ConstrainedFuture[E, A] = new ConstrainedFuture(
    EitherT(
      Future {
        try {ea} catch {case e: Throwable => recover(e).left}
      }
    )
  )
  /**
    * Construct a constrained future from an appropriate future
    */
  def future[E, A](fea: Future[E \/ A])(recover: Throwable => E)(implicit ec: ExecutionContext): ConstrainedFuture[E, A] = new ConstrainedFuture(
    EitherT(
      fea.recover {
        case e: Throwable =>
          \/.left[E, A](recover(e))
      }
    )
  )

  /**
    * A method for sequencing ConstrainedFutures
    * @param in - A collection of ConstrainedFuture[E, A]
    * @return A constrained future of a collection of As
    */
  def sequence[E, A, M[X] <: TraversableOnce[X]](in: M[E ConstrainedFuture A])
      (implicit cbf: CanBuildFrom[M[E ConstrainedFuture A], A, M[A]], ec: ExecutionContext): E ConstrainedFuture M[A] =
      in.foldLeft(immediatePoint[E, mutable.Builder[A, M[A]]](cbf(in))) { // ignore the red, this actually compiles
        (er, ea) => for {
          r <- er
          a <- ea
        } yield r += a
      }.map(_.result())

  /**
    * Switch around an option of a constrained future
    * @return
    */
  def switch[E, A](in: Option[E ConstrainedFuture A])(implicit ec: ExecutionContext): E ConstrainedFuture Option[A] =
    in.fold(immediatePoint[E, Option[A]](Option.empty[A]))(ea => ea.map(_.some))
}

