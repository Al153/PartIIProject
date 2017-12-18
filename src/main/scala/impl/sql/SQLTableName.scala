package impl.sql

import java.util.UUID

import core.schema.{RelationName, TableName}
import core.utils._

import scalaz.Monad


/**
  * A tablename type that is used to make sure that tablenames don't clash with the views table

  */
sealed trait SQLTableName {
  def name: String

  override def toString: String = name

  // Comparison is done by name only

  override def equals(obj: scala.Any): Boolean = obj match {
    case s: SQLTableName => this.name == s.name
    case _ => false
  }

  override def hashCode(): Int = name.hashCode
}

class ObjectTableName(in: TableName, index: Long) extends SQLTableName {
  override val name: String = "USERSPACE_" + in.value.strip + "_" + index
}
class RelationTableName(r: RelationName, index: Long) extends SQLTableName {
  override val name: String = "REL_" + r.id.strip + "_" + index
}
case object ViewsTableName extends SQLTableName {
  override def name: String = "VIEWS_ID"
}

case object ViewsRegistryName extends SQLTableName {
  override def name: String = "VIEWS_REGISTRY"
}

case object CommitsRegistryName extends SQLTableName {
  override def name: String = "COMMITS_REGISTRY"
}

case class PrecomputedView() extends SQLTableName {
  override val name: String = s"VIEW_${UUID.randomUUID()}"
}

case object DefaultsTableName extends SQLTableName {
  override def name: String = "DEFAULTS_TABLE"
}

case class TableNameFromDatabase(name: String) extends SQLTableName

object SQLTableName {
  class Context private[SQLTableName] (relations: Long, objects: Long) {
    private[SQLTableName] def newRel: (Long, Context) = (relations, new Context(relations+1, objects))
    private[SQLTableName] def newObj: (Long, Context) = (objects, new Context(relations, objects+1))
  }

  case class WithContext[A](runState: Context => (A, Context))

  implicit object ContextMonad extends Monad[WithContext] {
    override def bind[A, B](ma: WithContext[A])(f: (A) => WithContext[B]): WithContext[B] =
      WithContext(
        s => {
          val (a, ss) = ma.runState(s)
          f(a).runState(ss)
        }
      )

    override def point[A](a: => A): WithContext[A] = WithContext(s => (a, s))
  }

  def getName(t: TableName): WithContext[ObjectTableName] = WithContext(
    s => {
      val (index, ss) = s.newObj
      (new ObjectTableName(t, index), ss)
    }
  )


  def getName(r: RelationName): WithContext[RelationTableName] = WithContext(
    s => {
      val (index, ss) = s.newObj
      (new RelationTableName(r, index), ss)
    }
  )

  def render[A](w: WithContext[A]): A = w.runState(new Context(0, 0))._1


}