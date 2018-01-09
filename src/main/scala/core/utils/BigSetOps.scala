package core.utils

/**
  * Created by Al on 01/01/2018.
  */
trait BigSetOps {
  def bigUnion[A](sets: Seq[Set[A]]): Set[A] = sets.foldLeft(Set[A]()){_ union _}
  def bigIntersection[A](ifEmpty: Set[A], sets: Seq[Set[A]]): Set[A] =
    sets match {
      case head +: tail => tail.foldLeft(head){_ intersect _}
      case _ => ifEmpty
    }
}
