package impl.sql

import org.h2.engine.Session
import scalikejdbc._

/**
  * represents a read query
  */

trait ReadQueryString {
  def getString: String
  def execute(implicit session: Session) = DB readOnly {
    implicit session =>
      withSQL {
        select
      }
  }
}