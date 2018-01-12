package impl.sql.names

import core.backend.intermediate.RelationName
import core.user.schema.TableName
import core.utils._

import scalaz._


/**
  * A tablename type that is used to make sure that tablenames don't clash with the views table
  * sealed trait hierarchy
  */
sealed trait SQLTableName {
  def name: String

  override def toString: String = name

  /**
    * Comparison is done by name only
    */
  override def equals(obj: scala.Any): Boolean = obj match {
    case s: SQLTableName => this.name == s.name
    case _ => false
  }

  override def hashCode(): Int = name.hashCode
}

/**
  * Names for object tables
  * Index ensures that the name is unique
  */
class ObjectTableName private[names] (table: TableName, index: Long) extends SQLTableName {
  /**
    * Sanitises the underlying [{TableName]]
    *
    */
  override val name: String = "USERSPACE_" + table.value.strip + "_" + index
}

/**
  * Names for relation tables
  *  Index ensures the name is unique
  */
class RelationTableName private[names] (r: RelationName, index: Long) extends SQLTableName {
  /**
    * Sanitises the underlying [[RelationName]]
    */
  override val name: String = "REL_" + r.id.strip + "_" + index
}

/**
  * There is only one [[impl.sql.tables.ViewsTable]], so we have a single name for it
  */
case object ViewsTableName extends SQLTableName {
  override def name: String = "VIEWS_TABLE"
}

/**
  * There is only one [[impl.sql.tables.ViewsRegistry]], so we have a single name for it
  */
case object ViewsRegistryName extends SQLTableName {
  override def name: String = "VIEWS_REGISTRY"
}

/**
  * There is only one [[impl.sql.tables.CommitsRegistry]], so we have a single name for it
  */
case object CommitsRegistryName extends SQLTableName {
  override def name: String = "COMMITS_REGISTRY"
}

/**
  * TableName for the auxialliary table of an [[impl.sql.tables.ObjectTable]]
 */
case class AuxialliaryName(objectTable: SQLTableName) extends SQLTableName {
  override def name: String = "AUX" + objectTable.name
}

/**
  * There is only one [[impl.sql.tables.DefaultsTable]], so we have a single name for it
  */
case object DefaultsTableName extends SQLTableName {
  override def name: String = "DEFAULTS_TABLE"
}

/**
  * Case class for names of tables that have been extracted from the database, which could have any name
  */
case class TableNameFromDatabase(name: String) extends SQLTableName

/**
  * Monadic code for guaranteeing unique indices for table names (relations, objects)
  */
object SQLTableName {

  /**
    * Simple state class
    */
  class Context private[SQLTableName](relations: Long, objects: Long) {
    private[SQLTableName] def newRel: (Long, Context) = (relations, new Context(relations+1, objects))
    private[SQLTableName] def newObj: (Long, Context) = (objects, new Context(relations, objects+1))
  }

  /**
    * used as part of a state monad
    */
  type WithContext[A] = State[Context, A]

  /**
    * Get an ObjectTableName
   */

  def getName(t: TableName): WithContext[ObjectTableName] = State {
    s => {
      val (index, ss) = s.newObj
      (ss, new ObjectTableName(t, index))
    }
  }

  /**
    * Get a RelationTableName
    */

  def getName(r: RelationName): WithContext[RelationTableName] = State {
    s =>
      val (index, ss) = s.newObj
      (ss, new RelationTableName(r, index))
  }

  /**
    * Render the context
    */
  def render[A](w: WithContext[A]): A = w.run(new Context(0, 0))._2

  /**
    * Point function for state monad
    */
  def point[A](a: => A): WithContext[A] = State.apply(s => (s, a))

}