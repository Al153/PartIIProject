package db

import db.interfaces.DBInstance
import schema.SchemaDescription

/**
  * Created by Al on 07/10/2017.
  */
object DBOpen {
  def apply(path: DBPath, schema: SchemaDescription): DBInstance = ??? // todo: implement, and make schema more type safe

}
