package schema

import core.RelationAttributes

/**
  * Created by Al on 21/10/2017.
  *
  * Used by a backend to generate schema
  */
class SchemaDescription(val objects: Set[SchemaObject[Any]], val relations: Set[RelationAttributes[Any, Any]]) {
  // contains a set of SchemaObjects and a set of RelationalAttributes
}
