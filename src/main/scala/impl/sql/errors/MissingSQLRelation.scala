package impl.sql.errors

import impl.sql.RelationTableName

/**
  * Created by Al on 22/11/2017.
  */
case class MissingSQLRelation(name: RelationTableName) extends SQLError {

}
