package impl.sql.tables

import core.utils.{EitherOps, _}
import impl.sql.errors.{SQLSchemaColumnMissing, SQLSchemaLengthMismatch}
import impl.sql.names.{SQLColumnName, SQLTableName}
import impl.sql.schema._
import impl.sql.{SQLEither, SQLInstance}

import scalaz.Scalaz._
import scalaz._



/**
  * Created by Al on 10/12/2017.
  * [[SQLTable]] Implementors contain a number of methods for interacting with the table's counterpart in the PostgreSQL
  * DB
  */

trait SQLTable {

  /**
    * Pretty print the table name so that the table can be used in a
    * substitution $string
    */
  override def toString: String = name.toString

  /**
    * Check table exists, and create if it doesn't exist
    *
    * @param foundTables - tables that were found
    */
  def validateOrCreate(foundTables: Set[SQLTableName]): SQLEither[Unit] =
    if (name in foundTables) validateTable()
    else create


  /**
    * Creates a new instance of the table
    * @return
    */
  protected def create: SQLEither[Unit] = {
    val q = this.schema.create(name)
    instance.doWriteEither(q)
  }

  /**
    * Checks that the table's DB counterpart has the correct schema
    * @return
    */
  private def validateTable(): SQLEither[Unit] =  for {
    columns <- getColumns(name)
    _ <- validateColumns(columns, schema.components)
  } yield ()

  /**
    * Get the column schema from the SQL DB
    * @return
    */
  private def getColumns(name: SQLTableName): SQLEither[List[ColumnSpecification]] = {
    val query =
      s"""
         |SELECT COLUMN_NAME, DATA_TYPE
         |    FROM INFORMATION_SCHEMA.COLUMNS
         |    WHERE table_name = '${this.name}'
         |"""".stripMargin

    instance.reader.getColumns(query)
  }

  /**
    * Given a list of column specifications, make sure the table matches
    * @param columns - columns found
    * @param expected - expected columns
    * @return
    */
  private def validateColumns(columns: List[ColumnSpecification], expected: Map[SQLColumnName, SQLType]): SQLEither[Unit] = for {
    _ <- assertLength(columns, expected)
    _ <- EitherOps.sequence (for {
      column <- columns
      eType = expected.getOrError(column.name, SQLSchemaColumnMissing(column, expected))
    } yield eType.flatMap(checkColumn(column, _)))
  } yield ()

  /**
    * Assert there is the right number of extracted columns
    */
  private def assertLength(specifications: List[ColumnSpecification], nameToType: Map[SQLColumnName, SQLType]): SQLEither[Unit] =
    if (specifications.length == nameToType.size) ().right else SQLSchemaLengthMismatch().left


  /**
    * Check that the type of a column is correct
    */
  private def checkColumn(column: ColumnSpecification, expectedType: SQLType): SQLEither[Unit] =
    SQLType.validateType(expectedType, column.colType)

  /**
    * An SQL table should define a schema
    * @return
    */
  def schema: SQLSchema

  /**
    * An SQL table should define a name
    */
  def name: SQLTableName

  /**
    * An SQL table should contain a back-reference to the instance that holds it
    * @return
    */
  def instance: SQLInstance
}

