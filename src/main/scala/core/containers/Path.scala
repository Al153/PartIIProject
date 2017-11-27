package core.containers

import core.error.E
import core.intermediate.unsafe.ErasedPath
import core.schema.SchemaObject
import scalaz._, Scalaz._
import scalaz.\/

/**
  * Created by Al on 09/10/2017.
  */
abstract class Path[A](implicit sa: SchemaObject[A]) {
  def getSteps: Vector[(A, A)] // get all steps in the path. Todo: can we get more information out here
}

final class PathImpl[A](steps: Vector[(A, A)])(implicit sa: SchemaObject[A]) extends Path[A]()(sa) {
  override def getSteps: Vector[(A, A)] = steps
}

object Path {
  def from[A](e: ErasedPath, sa: SchemaObject[A]): E \/ Path[A] = {
    e.getSteps.foldLeft(Vector[(A, A)]().right[E]){
      case (ev, (d1, d2)) =>
        for {
          a1 <- sa.fromRow(d1)
          a2 <- sa.fromRow(d2)
          v <- ev
        } yield v :+ (a1, a2)
    }.map(new PathImpl(_)(sa))
  }

  def from[A](v: Vector[A])(implicit sa: SchemaObject[A]): Path[A] = new Path[A] {
    override val getSteps: Vector[(A, A)] = v.dropRight(1).zip(v.drop(1))
  }
}

