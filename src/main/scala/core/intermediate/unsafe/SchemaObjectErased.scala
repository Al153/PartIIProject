package core.intermediate.unsafe

import core.schema.{SchemaComponent, TableName}

/**
  * Created by Al on 26/10/2017.
  */
case class SchemaObjectErased(name: TableName, schemaComponents: Vector[SchemaComponent]) {
  def prototype: UnsafeFindable = UnsafeFindable(schemaComponents.map(_ => None), name)
  val length: Int = schemaComponents.length
}