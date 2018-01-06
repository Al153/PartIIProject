package core.user.dsl

import java.nio.file.{Path, Paths}

/**
  * Created by Al on 29/10/2017.
  */
sealed trait DatabaseAddress
case class DBDir(p: Path, user: String, password: String) extends DatabaseAddress
case object Empty extends DatabaseAddress

object DatabaseAddress {
  implicit class StringDBOps(s: String) {
    def db: DBDir = DBDir(Paths.get(s), "", "")
  }
}
