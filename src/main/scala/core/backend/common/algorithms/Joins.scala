package core.backend.common.algorithms

/**
  * Created by Al on 30/12/2017.
  *
  * Implementations of in-memory joins
  */
object Joins {
  def joinVector[A, B, C](leftRes: Vector[(A, B)], rightRes: Vector[(B, C)]): Vector[(A, C)] = {
    // build an index of all values to join, prevents overduplication
    val collectedLeft = leftRes.foldLeft(Map[B, Vector[A]]()) {
      case (m, pair) =>
        m + (pair._2 -> (m.getOrElse(pair._2, Vector[A]()) :+ pair._1))
    }

    for {
      (middle, to) <- rightRes
      from <- collectedLeft.getOrElse(middle, Vector())
    } yield (from, to)
  }


  def joinSet[A, B, C](leftRes: Set[(A, B)], rightRes: Set[(B, C)]): Set[(A, C)] = {
    // build an index of all values to join, prevents overduplication
    val collectedLeft = leftRes.foldLeft(Map[B, Set[A]]()) {
      case (m, pair) =>
        m + (pair._2 -> (m.getOrElse(pair._2, Set[A]()) ++ Set(pair._1)))
    }

    for {
      (middle, to) <- rightRes
      from <- collectedLeft.getOrElse(middle, Set())
    } yield (from, to)
  }
}
