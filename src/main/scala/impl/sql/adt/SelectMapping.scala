package impl.sql.adt

import impl.sql.names.SQLColumnName.{leftId, objId, rightId}

/**
  * Sealed trait hierarchy for the SELECT [[SelectMapping]] FROM ...
  * Part of an SQL query
  */
sealed trait SelectMapping

/**
  * Select * from ...
  */
case object All extends SelectMapping

/**
  * Select left_id, right_id from ...
  */
case object Simple extends SelectMapping

/**
  * Select (id as left_id, id as right_id) from ...
  */
case object FromObject extends SelectMapping

/**
  * select a.left_id as left_id, b.right_id as right_id
  */
case class Joined(a: VarName, b: VarName) extends SelectMapping

/**
  *  select a.left_id as left_id,  a.right_id as right_id
  */
case class SameSide(a: VarName) extends SelectMapping

/**
  * select r.right_id as left_id, r.left_id as right_id
  */
case class ReversedRelation(r: VarName) extends SelectMapping

/**
  * select ..... $limit + 1 as $limit
  */
case class WithLimit(limit: VarName, rest: SelectMapping) extends SelectMapping

/**
  * select ..... 0 as $limit
  */

case class StartLimit(limit: VarName, rest: SelectMapping) extends  SelectMapping

object SelectMapping {
  /**
    * Render by case matching
    */
  def render(s: SelectMapping): String = s match {
    case All => "*"
    case Simple => s"$leftId, $rightId"
    case FromObject => s"$objId AS $leftId, $objId AS $rightId"
    case Joined(a, b) => s"$a.$leftId AS $leftId, $b.$rightId AS $rightId"
    case SameSide(a) => s"$a.$leftId AS $leftId, $a.$rightId AS $rightId"
    case ReversedRelation(a) => s"$a.$rightId AS $leftId, $a.$leftId AS $rightId"
    case WithLimit(lim, rest) => s"${render(rest)}, $lim + 1 AS $lim"
    case StartLimit(lim, rest) => s"${render(rest)}, 0 AS $lim"
  }
}