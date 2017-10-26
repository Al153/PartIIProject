package examples

import core.dsl.Commands.find
import db._
import examples.Schema._
import view.View

import scalaz.\/-
import scala.concurrent.ExecutionContext.Implicits.global // global execution context


/**
  * Created by Al on 15/10/2017.
  */
class DatabasesExamples {
  implicit val instance = memory.instance

  def query: Unit = {
    using(DBOpen("/path/to/sql/database", Schema.description)){
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
