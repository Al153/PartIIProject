package unit

/**
  * Created by Al on 23/12/2017.
  */
package object suites {
  implicit def vectorOrdering[A](implicit oa: Ordering[A]): Ordering[Vector[A]] = new Ordering[Vector[A]] {
    override def compare(x: Vector[A], y: Vector[A]): Int =
      if (x.length != y.length) x.length - y.length
      else {
        def oStep(v1: Vector[A], v2: Vector[A]): Int = (v1, v2) match {
          case (h1 +: t1, h2 +: t2) =>
            val c = oa.compare(h1, h2)
            if (c != 0) c
            else oStep(t1, t2)
          case (Vector(), Vector()) => 0
          case (Vector(), _) => -1
          case _ => 1
        }

        oStep(x, y)
      }
  }
}
