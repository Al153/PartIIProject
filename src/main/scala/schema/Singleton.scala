package schema

import core.dsl.UnaryQuery
import core.intermediate

/**
  * Created by Al on 09/10/2017.
  */
case class Singleton() extends UnaryQuery[Singleton] {
  override def tree(a: intermediate.IntermediateTree[Singleton]): intermediate.IntermediateTree[(Singleton, Singleton)] =
}

