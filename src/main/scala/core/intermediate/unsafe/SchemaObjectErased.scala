package core.intermediate.unsafe

import schema.{SchemaComponent, TableName}

/**
  * Created by Al on 26/10/2017.
  */
case class SchemaObjectErased(name: TableName, schemaComponents: Vector[SchemaComponent])