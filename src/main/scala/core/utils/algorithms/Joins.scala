package core.utils.algorithms

import scala.collection.mutable

/**
  * Created by Al on 30/12/2017.
  *
  * Implementations of in-memory joins, using indices to increase speeds
  */
object Joins {
  /**
    * Joins a pair of vectors (multisets)
    * @param leftRes - left side
    * @param rightRes - right side
    * @tparam A - types of object to join
    * @tparam B - ditto
    * @tparam C - ditto
    * @return
    */
  def joinVector[A, B, C](leftRes: Vector[(A, B)], rightRes: Vector[(B, C)]): Vector[(A, C)] = {
    // build an index of all values to join, prevents overduplication.
    // mutable map for speed of writing
    val collectedLeft = leftRes.foldLeft(mutable.Map[B, Vector[A]]()) {
      case (m, pair) =>
        m + (pair._2 -> (m.getOrElse(pair._2, Vector[A]()) :+ pair._1))
    }

    for {
      (middle, to) <- rightRes
      from <- collectedLeft.getOrElse(middle, Vector())
    } yield (from, to)
  }

  /**
    * Joins a pair of sets
    * @param leftRes - left side
    * @param rightRes - right side
    * @tparam A - types of object to join
    * @tparam B - ditto
    * @tparam C - ditto
    * @return
    */


  def joinSet[A, B, C](leftRes: Set[(A, B)], rightRes: Set[(B, C)]): Set[(A, C)] = {
    // build an index of all values to join on right, since Proj_B(right) is a subset of Proj_B(Left)
    val collectedRight = mutable.Map[B, mutable.Set[C]]()
    for ((b, c) <- rightRes) {
      val s = collectedRight.getOrElseUpdate(b, mutable.Set())
      s += c
    }

    for {
      (left, middle) <- leftRes
      right <- collectedRight.getOrElse(middle, Set[C]())
    } yield (left, right)
  }
}
