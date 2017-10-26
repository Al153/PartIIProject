package schema

import core.intermediate.unsafe.{ErasedRelationAttributes, SchemaObjectErased}

/**
  * Created by Al on 21/10/2017.
  *
  * Used by a backend to generate schema
  */
class SchemaDescription(val objects: Set[SchemaObjectErased], val relations: Set[ErasedRelationAttributes]) {
  // contains a set of SchemaObjects and a set of RelationalAttributes
}




