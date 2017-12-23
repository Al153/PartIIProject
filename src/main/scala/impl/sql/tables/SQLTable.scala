package impl.sql.tables

import core.utils.{EitherOps, _}
import impl.sql.errors.{SQLSchemaColumnMissing, SQLSchemaLengthMismatch}
import impl.sql.schema._
import impl.sql.{SQLColumnName, SQLEither, SQLInstance, SQLTableName}

import scalaz.Scalaz._
import scalaz._



/**
  * Created by Al on 10/12/2017.
  *
  *
  * Model:
  *
  * Defaults table holds a single entry: default view
  *
  * Views registry: one column: all valid views
  * Commits registry: One column: all valid commits
  * Views Table: Maps Views -> commits
  *
  * Relation Table: (Commit_id, left_id, right_id)
  * Object Tables: (obj_id, arbitrary columns)
  */

// Todo: need to keep track of which views an object is visible from
trait SQLTable {

  override def toString: String = name.toString

  // Check table exists, and create if it doesn't exist
  def validateOrCreate(foundTables: Set[SQLTableName]): SQLEither[Unit] =
    if (name in foundTables) validateTable()
    else create


  protected def create: SQLEither[Unit] = {
    instance.doWriteEither(this.schema.create(name))
  }

  private def validateTable(): SQLEither[Unit] =  for {
    columns <- getColumns(name)
    _ <- validateColumns(columns, schema.components)
  } yield ()

  private def getColumns(name: SQLTableName): SQLEither[List[ColumnSpecification]] = {
    val query =
      s"""
         |SELECT COLUMN_NAME, DATA_TYPE
         |    FROM INFORMATION_SCHEMA.COLUMNS
         |    WHERE table_name = '${this.name}'
         |"""".stripMargin

    instance.reader.getColumns(query)
  } // needs to extract column info from a table

  private def validateColumns(columns: List[ColumnSpecification], expected: Map[SQLColumnName, SQLType]): SQLEither[Unit] = for {
    _ <- assertLength(columns, expected)
    _ <- EitherOps.sequence (for {
      column <- columns
      eType = expected.getOrError(column.name, SQLSchemaColumnMissing(column, expected))
    } yield eType.flatMap(checkColumn(column, _)))
  } yield ()

  private def assertLength(specifications: List[ColumnSpecification], nameToType: Map[SQLColumnName, SQLType]): SQLEither[Unit] =
    if (specifications.length == nameToType.size) ().right else SQLSchemaLengthMismatch().left


  /**
    * Check that the type is correct
    */
  private def checkColumn(column: ColumnSpecification, expectedType: SQLType): SQLEither[Unit] =
    SQLType.validateType(expectedType, column.colType)

  def schema: SQLSchema
  def name: SQLTableName
  def instance: SQLInstance
}

