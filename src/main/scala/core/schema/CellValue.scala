package core.schema

/**
  * Created by Al on 20/10/2017.
  */
sealed trait CellValue
case class Cell[A](a: A)(implicit t: Storeable[A]) extends CellValue {}
