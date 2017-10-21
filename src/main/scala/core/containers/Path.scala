package core.containers

import schema.SchemaObject

/**
  * Created by Al on 09/10/2017.
  */
trait Path[A] {
  def getStart(implicit sa: SchemaObject[A]): A // todo: proper type bounds
  def getSteps(implicit sa: SchemaObject[A]): Vector[(A, A)] // get all steps in the path. Todo: can we get more information out here
}
