package examples

import core.user.dsl.DatabaseAddress._
import core.user.dsl._
import core.user.interfaces.DBInstance
import examples.Schema._
import impl.memory.MemoryDB

import scala.concurrent.ExecutionContext.Implicits.global
import scalaz.\/-


/**
  * Created by Al on 15/10/2017.
  */
class DatabasesExamples {

  def query(implicit instance: DBInstance): Unit = {
    using(instance){
      for {
         actors <- find(coactor.from(jenniferLawrence))
      } yield {
        actors.filter(_ != jenniferLawrence).groupBy(identity).mapValues(_.size)
      }
    }.andThen { case \/-(m)=> m.foreach{case (Actor(name), count) => println(s"$name: $count")}}
  }
}
