package core.user.containers

import core.backend.common.ExtractError
import core.backend.intermediate.unsafe.ErasedPath
import core.user.schema.SchemaObject

import scalaz.Scalaz._
import scalaz.{\/, _}

/**
  * Created by Al on 09/10/2017.
  */
abstract class Path[A] {
  def getSteps: Vector[(A, A)] // get all steps in the path
}

final class PathImpl[A](steps: Vector[(A, A)])extends Path[A]() {
  override def getSteps: Vector[(A, A)] = steps
}

object Path {
  def from[A](e: ErasedPath, sa: SchemaObject[A]): ExtractError \/ Path[A] = {
    e.getSteps.foldLeft(Vector[(A, A)]().right[ExtractError]){
      case (ev, (d1, d2)) =>
        for {
          a1 <- sa.fromRow(d1)
          a2 <- sa.fromRow(d2)
          v <- ev
        } yield v :+ (a1, a2)
    }.map(new PathImpl(_))
  }

  def fromVector[A](v: Vector[A]): Path[A] = new Path[A] {
    override val getSteps: Vector[(A, A)] = v.dropRight(1).zip(v.drop(1))
  }
}

