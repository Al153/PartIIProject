package examples

import core.backend._
import core.backend.interfaces.DatabaseAddress._
import core.dsl.Commands.find
import examples.Schema._
import impl.memory.MemoryDB

import scala.concurrent.ExecutionContext.Implicits.global
import scalaz.\/-


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
