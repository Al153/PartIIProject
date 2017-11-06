package core

import core.error.E

import scala.collection.MapLike
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scalaz.Scalaz._
import scalaz.\/

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

  implicit class SetOps2[E, A](u: Set[E \/ Set[A]]) {
    def flattenE: E \/ Set[A] = EitherOps.sequence(u).map(_.flatten)
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

  implicit class StringifyOps(u: Any) {
    def truncate(len: Int): String =   String.format("%1$9s", u.toString)
  }

  implicit class MapLikeOps[K, A](u: MapLike[K, A, _]) {
    def getOrError(k: K, e: => E): E \/ A = u.get(k).fold(\/.left[E, A](e))(_.right[E])
  }

  implicit class MaplikePrefixOps[A](a: A) {
    def in[B](m: MapLike[A, B, _]): Boolean = m.contains(a)
  }
}
