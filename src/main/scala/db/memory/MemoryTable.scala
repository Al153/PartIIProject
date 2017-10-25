package db.memory

/**
  * Created by Al on 25/10/2017.
  */

sealed trait MemoryTable {
  def objects: Vector[MemoryObject]
}

