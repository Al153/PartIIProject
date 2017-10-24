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
}
