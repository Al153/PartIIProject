package core

import core.error.E

import scala.collection.{MapLike, SetLike, TraversableLike, mutable}
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scalaz.Scalaz._
import scalaz.\/

/**
  * Created by Al on 24/10/2017.
  */
package object utils {
  implicit class SetOps[A](u: Set[A]) {
     def mapPair: Set[(A, A)] = u.map(x => (x, x))
  }

  implicit class SetOps2[E, A](u: Set[E \/ Set[A]]) {
    def flattenE: E \/ Set[A] = EitherOps.sequence(u).map(_.flatten)
  }

  implicit class VectorOps2[E, A](u: Vector[E \/ TraversableOnce[A]]) {
    def flattenE: E \/ Vector[A] = EitherOps.sequence(u).map(_.flatten)
  }


  implicit class TraversibleOps[M[X] <: TraversableOnce[X], A](u: M[A]) {
    private def pair(a: A): (A, A) = (a, a)

    def mapPair(implicit cbf: CanBuildFrom[M[A], (A, A), M[(A, A)]]): M[(A, A)] =
      u.foldLeft(cbf(u)){
        case (builder, a) => builder += pair(a)
      }.result()
  }

  implicit class TraversibleOps2[M[X] <: TraversableOnce[X], Y, Z](m: M[(Y, Z)]) {
    def mapProj1(implicit cbf: CanBuildFrom[M[(Y, Z)], Y, M[Y]]): M[Y] = {
      m.foldLeft(cbf(m)) {
        case (builder, (y, _)) =>
          builder += y
      }.result()
    }

    def mapProj2(implicit cbf: CanBuildFrom[M[(Y, Z)], Z, M[Z]]): M[Z] = {
      m.foldLeft(cbf(m)) {
        case (builder: mutable.Builder[Z, M[Z]], (_, z)) =>
          builder += z
      }.result()
    }
  }

  implicit class TraversibleOps3[M[X] <: TraversableOnce[X], A, B, C](m: M[(A, B, C)]) {
    def mapProj1(implicit cbf: CanBuildFrom[M[(A, B, C)], A, M[A]]): M[A] = {
      m.foldLeft(cbf(m)) {
       case (builder, (a, _, _)) =>
          builder += a
      }.result()
    }

    def mapProj2(implicit cbf: CanBuildFrom[M[(A, B, C)], B, M[B]]): M[B] = {
      m.foldLeft(cbf(m)) {
        case (builder, (_, b, _)) =>
          builder += b
      }.result()
    }

    def mapProj3(implicit cbf: CanBuildFrom[M[(A, B, C)], C, M[C]]): M[C] = {
      m.foldLeft(cbf(m)) {
        case (builder, (_, _, c)) =>
          builder += c
      }.result()
    }
  }

  object EitherOps {
    def sequence[E, A, M[X] <: TraversableOnce[X]]
    (in: M[E \/ A])
    (implicit cbf: CanBuildFrom[M[E \/ A], A, M[A]]): E \/ M[A] =
      in.foldLeft(cbf(in).right[E]) { // ignore the red, this actually compiles
        (er, ea) => for {
          r <- er
          a <- ea
        } yield r += a
      }.map(_.result())
  }

  implicit class EitherOps1[E, A](u: E \/ A) {
    def withSnd[B](b: B): E \/ (A, B) = u.map((_, b))
    def withFst[B](b: B): E \/ (B, A) = u.map((b, _))
  }

  implicit class StringifyOps(u: Any) {
    def truncate(len: Int): String =   String.format("%1$9s", u.toString)
  }

  implicit class MapLikeOps[K, A](u: MapLike[K, A, _]) {
    def getOrError(k: K, e: => E): E \/ A = u.get(k).fold(\/.left[E, A](e))(_.right[E])
  }

  implicit class MaplikePrefixOps[A](a: A) {
    def in[B](m: MapLike[A, B, _]): Boolean = m.contains(a)
    def notIn[B](m: MapLike[A, B, _]): Boolean = !m.contains(a)
    def in[B](s: SetLike[A, _]): Boolean = s.contains(a)
    def notIn[B](s: SetLike[A, _]): Boolean = !s.contains(a)
  }


  implicit class StringOps(s: String) {
    def strip: String = s.replaceAll("[\\W]|_", "")
  }
}
