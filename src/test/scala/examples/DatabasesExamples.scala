package examples

import core.dsl.Commands.find
import db._
import db.memory.MemoryDB
import examples.Schema._

import scala.concurrent.ExecutionContext.Implicits.global
import scalaz.\/- // global execution context
import db.interfaces.DatabaseAddress._


/**
  * Created by Al on 15/10/2017.
  */
class DatabasesExamples {

  def query: Unit = {
    using(MemoryDB.open("/path/to/database".db, Schema.description)){
      implicit instance =>
        for {
          actors <- find(coactor.from(jenniferLawrence))
        } yield {
          actors.filter(_ != jenniferLawrence).groupBy(identity).mapValues(_.size)
        }
    }.andThen { case \/-(m)=> m.foreach{case (Actor(name), count) => println(s"$name: $count")}}
  }
}
