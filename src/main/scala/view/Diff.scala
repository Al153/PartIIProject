package view

import core.NodeDef
import core.relations.Relation
import core.error.E
import core.containers.Operation

import scalaz.Monoid

/**
  * Created by Al on 16/10/2017.
  *
  * represents the diff of a pair of views
  *
  * not commutative:
  */
trait Diff {
  def +(d: Diff): Diff
  def -(d: Diff): Diff

  def positive: Seq[Relation[NodeDef, NodeDef]]
  def negative: Seq[Relation[NodeDef, NodeDef]]

  def apply: Operation[E, Unit]

}

object Diff {
  val Zero: Diff = new Diff {
    override def positive: Seq[Relation[NodeDef, NodeDef]] = Seq()
    override def negative: Seq[Relation[NodeDef, NodeDef]] = Seq()

    override def +(d: Diff): Diff = ???
    override def -(d: Diff): Diff = ???

    override def apply: Operation[E, Unit] = ???
  }
  implicit object DiffMonoid = new Monoid[Diff] {
    override def zero: Diff = Zero
    override def append(f1: Diff, f2: => Diff): Diff = f1 + f2
  }
}