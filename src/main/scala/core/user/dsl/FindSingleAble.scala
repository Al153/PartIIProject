package core.user.dsl

import core.backend.intermediate.FindSingle

import scala.language.higherKinds

/**
  * Created by Al on 13/01/2018.
  */

trait FindSingleAble[A] {
  def toFindSingle: FindSingle[A]
}