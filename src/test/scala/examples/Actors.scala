package examples

import core.concrete.relations.CompletedRelation
import core.dsl.Commands._
import core.{RelationAttributes, Singleton}
import db._
import db.memory.MemoryDB
import examples.Schema.{Borders, Country}

import scala.concurrent.ExecutionContext.Implicits.global
import db.interfaces.DatabaseAddress._
import scalaz.\/- // global execution context

/**
  * Created by Al on 15/10/2017.
  *
  * Issues/todo:
  * we want to add filters on the inside of nodes
  */
class Actors {

  /*
    * Define the schema we expect from the database
    * This might be loaded from a file or a library
   */


  def query(): Unit = {

    import Schema._




     /*
      * This is the interesting bit
      * a long, chained, expression that opens up a database instant, gets the default view, then finds all Actors with a chain of 4 unique coactor relationships to Tom Cruise
      * then filters the results for those whose names begin with "A" and then adds those passing the filter to the set
      * LinkedToTomCruise.
      * finally, from the same view, we collect all of the actors in the set LinkedToTomCruise using a predefined query, and print their names
      */


    using(MemoryDB.open("/path/to/sql/database".db, Schema.description)){

      for {
          actors <- find((coactor |*| 4).from(tomCruise))
          namedActors = actors.filter(actor => actor.name.startsWith("A"))
          _ <- insert(namedActors.map(actor => CompletedRelation(point, LinkedToTomCruise: RelationAttributes[Singleton, Actor], actor)))
          currentLinked <- find(getLinked.from(point))
        } yield currentLinked
    }.andThen {case \/-(actors) => actors.foreach(println)}
  }

  def paths(): Unit = {
    using(MemoryDB.open("/path/to/database".db, Schema.description)){
      allShortestPaths(jenniferLawrence, coactor)
    }.andThen {case \/-(paths) => paths.foreach(println)}
  }

  def borders(): Unit = {
    using(MemoryDB.open("/path/to/database".db, Schema.description)){
      for {
          pairs <- findPairsDistinct(Borders.*.tree)
          _     <- insert(pairs.map{case (country1, country2) => CompletedRelation(country1, Borders: RelationAttributes[Country, Country], country2)})
        } yield ()
    }
  }
}
