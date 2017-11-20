package impl.sql.adt

sealed trait SelectMapping
case object All extends SelectMapping // Select * from ...
case object Simple extends SelectMapping // Select left_id, right_id from ...
case object FromObject extends SelectMapping // Select (id as left_id, id as right_id) from ...
case class Joined(a: VarName, b: VarName) extends SelectMapping // select a.left_id as left_id, b.right_id as right_id
case class SameSide(a: VarName) extends SelectMapping // select a.left_id as left_id,  a.right_id as right_id
case class ReversedRelation(r: VarName) extends SelectMapping // select r.right_id as left_id, r.left_id as right_id
case class WithLimit(limit: VarName, rest: SelectMapping) extends SelectMapping // select ..... $limit + 1 as $limit
case class StartLimit(limit: VarName, rest: SelectMapping) extends  SelectMapping // select ..... 0 as $limit
