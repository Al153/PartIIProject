package core.backend.intermediate

/**
  * Created by Al on 17/10/2017.
  *
  * A sealed hierarchy to build schema for table based databases
  */
trait SchemaComponent {}
case object IntCell extends SchemaComponent
case object StringCell extends SchemaComponent
case object BoolCell extends SchemaComponent
case object DoubleCell extends SchemaComponent