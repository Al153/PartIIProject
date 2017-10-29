package core.containers

import core.error.E
import core.intermediate.unsafe.ErasedPath
import schema.SchemaObject

import scalaz.\/

/**
  * Created by Al on 09/10/2017.
  */
abstract class Path[A](implicit sa: SchemaObject[A]) {
  def getStart: A
  def getEnd: A
  def getSteps: Vector[(A, A)] // get all steps in the path. Todo: can we get more information out here
}

object Path {
  def from[A](e: ErasedPath, sa: SchemaObject[A]): E \/ Path[A] = ???
}

