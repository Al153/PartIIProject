package impl.sql.tables

import core.error.E
import core.utils.EitherOps
import impl.sql.{SQLColumnName, SQLTableName}
import impl.sql.schema._
import core.utils._
import impl.sql.errors.{SQLSchemaColumnMissing, SQLSchemaLengthMismatch, SQLSchemaTypeMismatch}

import scalaz._
import Scalaz._



/**
  * Created by Al on 10/12/2017.
  */
trait SQLTable {
  def validateTable(): E \/ Unit =  for {
    columns <- getColumns(name)
    _ <- validateColumns(columns, schema.components)
  } yield ()

  private def getColumns(name: SQLTableName): E \/ List[ColumnSpecification] = ??? // needs to extract column info from a table

  private def validateColumns(columns: List[ColumnSpecification], expected: Map[SQLColumnName, SQLType]): E \/ Unit = for {
    _ <- assertLength(columns, expected)
    _ <- EitherOps.sequence (for {
      column <- columns
      eType = expected.getOrError(column.getName, SQLSchemaColumnMissing(column, expected))
    } yield eType.flatMap(checkColumn(column, _)))
  } yield ()

  private def assertLength(specifications: List[ColumnSpecification], nameToType: Map[SQLColumnName, SQLType]): E \/ Unit =
    if (specifications.length == nameToType.size) ().right else SQLSchemaLengthMismatch().left


  /**
    * Check that the type is correct
    */
  private def checkColumn(column: ColumnSpecification, expectedType: SQLType): E \/ Unit =
    if (column.getType == expectedType) ().right else SQLSchemaTypeMismatch(column, expectedType).left

  def schema: SQLSchema
  def name: SQLTableName
}

