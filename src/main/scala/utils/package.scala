import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scalaz.Scalaz._
import scalaz.{\/, _}

/**
  * Created by Al on 24/10/2017.
  */
package object utils {
  implicit class PairVectorOps[A, B](u: Vector[(A, B)]) {
    def mapProj1: Vector[A] = u.map(_._1)
    def mapProj2: Vector[B] = u.map(_._2)
  }

  implicit class PairSetOps[A, B](u: Set[(A, B)]) {
    def mapProj1: Set[A] = u.map(_._1)
    def mapProj2: Set[B] = u.map(_._2)
  }

  implicit class SetOps[A](u: Set[A]) {
    def mapPair: Set[(A, A)] = u.map(x => (x, x))
  }

  implicit class VectorOps[A](u: Vector[A]) {
    def mapPair: Vector[(A, A)] = u.map(x => (x, x))
  }

  implicit class EitherOps[E, A](u: E \/ A) {
  }

  object EitherOps {
    def sequence[E, A, M[X] <: TraversableOnce[X]](in: M[E \/ A])(implicit cbf: CanBuildFrom[M[E \/ A], A, M[A]]): E \/ M[A] = {
      in.foldLeft(cbf(in).right[E]) { // ignore the red, this actually compiles
        (er, ea) => for {
          r <- er
          a <- ea
        } yield r += a
      }.map(_.result())
    }
  }
}
