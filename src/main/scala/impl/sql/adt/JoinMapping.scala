package impl.sql.adt

// can infer variable names from context
sealed trait JoinMapping
case object Chained extends JoinMapping // Join ... on a.right_id = b.left_id
case object OnRight extends JoinMapping // join ... on a.right_id = b.right_id