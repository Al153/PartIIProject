package impl.lmdb.retrieval

import core.schema.{RelationName, TableName}
import impl.lmdb.access.{Commit, ObjId}

/**
  * Created by Al on 28/12/2017.
  *
  * Destination of a relation as read and written to the db
  */
case class RelationDestination(relationName: RelationName, tableName: TableName, objectId: ObjId) {

}
