package examples

import core.dsl.Commands.{find, insert}
import schema.Pattern.?
import view.View
import Schema._
import db._

import scalaz.\/-

/**
  * Created by Al on 15/10/2017.
  */
class DatabasesExamples {
  def query: Unit = {
    using(DBOpen("/path/to/sql/database", Schema)){
      view: View => view.execute(
        for {
          actors <- find(coactor.from(jenniferLawrence))
        } yield {
          actors.filter(_ != jenniferLawrence).groupBy(identity).mapValues(_.size)
        }
      ).andThen { case \/-((m, _))=> m.foreach{case (Actor(name), count) => println(s"$name: $count")}}
    }
  }
}
