package core.user.dsl

import java.nio.file.{Path, Paths}

/**
  * Created by Al on 29/10/2017.
  *
  *  Sealed trait hierarchy for where to open a [[core.user.interfaces.DBInstance]]
  */
sealed trait DatabaseAddress

/**
  * Open a DB at the directory, using the supplied user and password (for SQL)
  */
case class DBDir(p: Path, user: String, password: String) extends DatabaseAddress

/**
  * Open a temporary database
  */
case object Empty extends DatabaseAddress

object DatabaseAddress {

  /**
    * String syntax for building a [[DatabaseAddress]]
    */
  implicit class StringDBOps(s: String) {
    def db: DBDir = DBDir(Paths.get(s), "", "")
  }
}
