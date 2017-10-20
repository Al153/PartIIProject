package schema

import view.Commit

/**
  * Created by Al on 20/10/2017.
  */
case class DatabaseRow(tableName: TableName, commit: Commit, fields: Map[ColumnName, CellValue])

