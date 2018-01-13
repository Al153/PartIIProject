package core.user.dsl

import core.backend.intermediate.FindPair

/**
  * Created by Al on 13/01/2018.
  */
trait FindPairAble[A, B] {
  def toFindPair: FindPair[A, B]
}
