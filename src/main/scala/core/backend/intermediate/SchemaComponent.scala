package core.backend.intermediate

/**
  * Created by Al on 17/10/2017.
  */
trait SchemaComponent {}
case object IntCell extends SchemaComponent
case object StringCell extends SchemaComponent
case object BoolCell extends SchemaComponent
case object DoubleCell extends SchemaComponent