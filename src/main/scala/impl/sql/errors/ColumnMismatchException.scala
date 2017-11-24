package impl.sql.errors

import core.schema.{SchemaComponent, TableName}

case class ColumnMismatchException(
                                    index: Int,
                                    table: TableName,
                                    prototype: Vector[SchemaComponent]
                                  ) extends SQLError {
  override def toString: String = s"Didn't expect a column index $index in table: ${table.value}(${prototype.mkString(", ")})"
}