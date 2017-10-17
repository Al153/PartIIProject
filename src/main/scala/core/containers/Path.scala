package core.containers

import core.{NodeDef, RelationAttributes}

/**
  * Created by Al on 09/10/2017.
  */
trait Path {
  def getStart[A <: NodeDef]: A // todo: proper type bounds
  def getSteps[A <: NodeDef, B <: NodeDef]: Vector[(RelationAttributes[A, B], NodeDef)] // get all steps in the path
}
