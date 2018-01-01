package impl.lmdb

/**
  * Created by Al on 01/01/2018.
  */
object BigSetOps {
  def bigUnion[A](sets: Seq[Set[A]]): Set[A] = sets.foldLeft(Set[A]()){_ union _}
  def bigIntersection[A](ifEmpty: Set[A], sets: Seq[Set[A]]): Set[A] = sets.foldLeft(ifEmpty){_ union _}

}
