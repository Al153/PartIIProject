package core.user.containers

import core.backend.common.ExtractError
import core.backend.intermediate.unsafe.ErasedPath
import core.user.schema.SchemaObject

import scalaz.Scalaz._
import scalaz.{\/, _}

/**
  * Created by Al on 09/10/2017.
  *
  *  A path is effectively a non-empty vector of pairs, such that the left
  *  entry of one pair matches the right entry of hte one before it
  */
abstract class Path[A] {
  /**
    * Get the steps of the path
    */
  def getSteps: Vector[(A, A)] // get all steps in the path
  /**
    * Number of steps in the path
    * @return
    */
  def length: Int = getSteps.length

  /**
    * Get the last object in the path
    * @return
    */
  def end: A = getSteps.last._2

  override def toString: String = getSteps.toString()
}

/**
  * A basic path impl
  * @param steps
  * @tparam A
  */
// todo: should verify steps is at least 1 long
final class PathImpl[A](steps: Vector[(A, A)])extends Path[A]() {
  override def getSteps: Vector[(A, A)] = steps
}

object Path {
  /**
    * Un-erase a path
    */
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

  /**
    * Create a path from a vector of objects
    * @param v
    * @tparam A
    * @return
    */
  def fromVector[A](v: Vector[A]): Path[A] = new Path[A] {
    override val getSteps: Vector[(A, A)] = v.dropRight(1).zip(v.drop(1))
  }
}

