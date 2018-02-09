package core.user.containers

import core.user.dsl.HasRecovery

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
class ConstrainedFuture[E, A] private (private val underlying: EitherT[Future, E, A])(implicit val ec: ExecutionContext, R: HasRecovery[E]) {
  /**
    * Standard map method
    */
  def map[B](f: A => B): ConstrainedFuture[E, B] = new ConstrainedFuture(underlying.map(f))
  /**
    * Standard flatMap method
    */
  def flatMap[B](f: A => ConstrainedFuture[E, B]) = new ConstrainedFuture(underlying.flatMap(a => f(a).underlying))

  // Magic recovery function
  private def doRecovery(e: Throwable): E =
    try {
      R.recover(e)
    } catch {
      case e: Throwable => doRecovery(e)
    }

  /**
    * @return the underlying Future[E \/ A]
    */
  def run: Future[E \/ A] = underlying.run.recover{case e => doRecovery(e).left}

  /**
    * Map on the left parameter
    */

  def leftMap[E1](f: E => E1)(implicit R1: HasRecovery[E1]) = new ConstrainedFuture(underlying.leftMap(f))

  /**
    * Recover an error in a constrained future: E
    */
  def recoverWith[E1](f: E => ConstrainedFuture[E1, A])(implicit R1: HasRecovery[E1]) = new ConstrainedFuture(
    EitherT(
      underlying.run.flatMap(
        ea => ea.fold(e => f(e).run, a => Promise.successful(a.right).future)
      )
    )
  )

  /**
    * Recover an error: E
    */
  def recover(f: E => A) = new ConstrainedFuture(
    EitherT(
      underlying.run.map(
        ea => ea.recover{case e => f(e)}
      )
    )
  )



  /**
    * Chain side effecting functions
    */
  def andThen(pf: PartialFunction[E \/ A, Unit]): ConstrainedFuture[E, A] =
    new ConstrainedFuture(EitherT(underlying.run.andThen{case scala.util.Success(ea) => pf(ea)}))
}

object ConstrainedFuture {
  /**
    * Construct a [[ConstrainedFuture]] simply
   */
    def point[E, A](a: => A)(implicit ec: ExecutionContext, R: HasRecovery[E]): ConstrainedFuture[E, A] = new ConstrainedFuture(
      EitherT(
        Future(
            a.right
        )
      )
    )

  /**
    * Construct a [[ConstrainedFuture]] where we know there are no errors
    */
  private def immediatePoint[E, A](a: A)(implicit ec: ExecutionContext, R: HasRecovery[E]): ConstrainedFuture[E, A] =
    new ConstrainedFuture(
      EitherT(
        Promise.successful(a.right[E]).future
      )
    )

  /**
    * Construct a constrained future from an appropriate either
    */
  def either[E, A](ea: => E \/ A)(implicit ec: ExecutionContext, R: HasRecovery[E]): ConstrainedFuture[E, A] = new ConstrainedFuture(
    EitherT(
      Future {
        ea
      }
    )
  )
  /**
    * Construct a constrained future from an appropriate future
    */
  def future[E, A](fea: Future[E \/ A])(implicit ec: ExecutionContext, R: HasRecovery[E]): ConstrainedFuture[E, A] = new ConstrainedFuture(
    EitherT(
      fea
    )
  )

  /**
    * A method for sequencing ConstrainedFutures
    * @param in - A collection of ConstrainedFuture[E, A]
    * @return A constrained future of a collection of As
    */
  def sequence[E, A, M[X] <: TraversableOnce[X]](in: M[E ConstrainedFuture A])
      (implicit cbf: CanBuildFrom[M[E ConstrainedFuture A], A, M[A]], ec: ExecutionContext, R: HasRecovery[E]): E ConstrainedFuture M[A] =
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
  def switch[E, A](in: Option[E ConstrainedFuture A])(implicit ec: ExecutionContext, R: HasRecovery[E]): E ConstrainedFuture Option[A] =
    in.fold(immediatePoint[E, Option[A]](Option.empty[A]))(ea => ea.map(_.some))
}

