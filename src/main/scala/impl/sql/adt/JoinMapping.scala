package impl.sql.adt

import impl.sql.names.SQLColumnName.{leftId, rightId}

// can infer variable names from context
sealed trait JoinMapping
case object Chained extends JoinMapping // Join ... on a.right_id = b.left_id
case object OnRight extends JoinMapping // join ... on a.right_id = b.right_id
case object OnLeft extends JoinMapping // join ... on a.leftId = b.right_id

object JoinMapping {
  def render(joinMapping: JoinMapping, a: VarName, b: VarName): String = joinMapping match {
    case Chained => s"$a.$rightId = $b.$leftId"
    case OnRight =>s"$a.$rightId = $b.$rightId"
    case OnLeft => s"$a.$leftId = $b.$rightId" // result of a single query (the b component is always on the right
  }
}