package impl.sql.compile

import core.intermediate.unsafe.ErasedRelationAttributes
import impl.sql.adt.{SQLDefinition, SQLPair}
import core.utils._

/**
  * Compilation context
  */

case class VarName(s: String) extends AnyVal {
  override def toString: String = s }
case class RelName(s: String) extends AnyVal
case class ContextState(
                         nameCount: Int,
                         relationDefinitions: Map[ErasedRelationAttributes, (RelName, SQLDefinition)],
                         bodyDefinitions: Map[VarName, SQLDefinition]
                       )

sealed trait Context[A] {
  import Context._

  def runState(state: ContextState): (A, ContextState)

  def map[B](f: A => B): Context[B] = _map(this, f)
  def flatMap[B](f: A => Context[B]): Context[B] = _flatMap(this, f)
}

// put a value into the state
case class Bind(body: SQLPair) extends Context[VarName] {
  override def runState(state: ContextState): (VarName, ContextState) = {
    val index = state.nameCount
    val newNameCount = state.nameCount + 1
    val name = VarName("Label" + index)
    val newDef = SQLDefinition.createDef(name, body)
    (name, ContextState(newNameCount, state.relationDefinitions, state.bodyDefinitions + (name -> newDef)))
  }
}

case class BindOrGetRel(rel: ErasedRelationAttributes) extends Context[RelName] {
  override def runState(state: ContextState): (RelName, ContextState) = {
    if (rel in state.relationDefinitions) {
      (state.relationDefinitions(rel)._1, state)
    } else {
      val index = state.nameCount
      val newNameCount = state.nameCount + 1
      val name = RelName("Rel" + index)
      val newDef = SQLDefinition.createDef(name, rel)
      (name, ContextState(newNameCount, state.relationDefinitions + (rel -> (name, newDef)), state.bodyDefinitions ))

    }
  }
}


// lookup a value in the state
case class Lookup(rel: ErasedRelationAttributes) extends Context[Option[VarName]] {
  override def runState(state: ContextState): (Option[VarName], ContextState) =
    (state.relationDefinitions.get(rel).map(_.varName) , state)
}

object Context {
  private def _map[A, B](outer: Context[A], f: A => B): Context[B] = new Context[B] {
    override def runState(state: ContextState): (B, ContextState) = {
      val (a, s1) = outer.runState(state)
      (f(a), s1)
    }
  }

  private def _flatMap[A, B](outer: Context[A], f: A => Context[B]) = new Context[B] {
    override def runState(state: ContextState): (B, ContextState) = {
      val (a, s1) = outer.runState(state)
      f(a).runState(s1)
    }
  }

  def point[A](a: => A) = new Context[A] {
    override def runState(state: ContextState): (A, ContextState) = (a, state)
  }
}