package construction.imdb

import construction.imdb.IMDBSchema._
import core.user.containers.{Operation, Path}
import core.user.dsl._
import core.user.interfaces.DBInstance
import core.utils._
import impl.lmdb.LMDB
import impl.memory.MemoryDB
import impl.sql.SQLDB
import spray.json._

import scala.language.implicitConversions
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.Source
import scalaz.Scalaz._


/**
  * Created by Al on 04/01/2018.
  */
object DBBuilder {

  def recover(e: Throwable): E = new E {
    override def toString: String = e.toString
  }

  def getNodes(): (Map[Int, JSONPerson], Map[Int, JSONMovie]) = {
    val lines = Source.fromResource("imdb/nodes.json")
    val allObjects = lines.mkString.parseJson
    val JsArray(objects) = allObjects

    val movies = objects.collect {case JsObject(m) if ("type" in m) && (m("type") == JsString("Movie")) => JSONMovie(m)}.map {m => m.key -> m}.toMap
    val people = objects.collect {case JsObject(p) if ("type" in p) && (p("type") == JsString("Person")) => JSONPerson(p)}.map {p => p.key -> p}.toMap

    (people, movies)
  }

  def getEdges(): (Set[JSONDirected], Set[JSONActed]) = {
    val allObjects = Source.fromResource("imdb/edges.json").mkString.parseJson
    val JsArray(objects) = allObjects

    val directed = objects.collect {case JsObject(d) if ("$label" in d) && (d("$label") == JsString("DIRECTED")) => JSONDirected(d)}.toSet
    val acted = objects.collect {case JsObject(a) if ("$label" in a) && (a("$label") == JsString("ACTS_IN")) => JSONActed(a)}.toSet
    (directed, acted)
  }


  def buildDB(implicit instance: DBInstance): Operation[E, Unit] = {
    val (actors, movies) = getNodes()
    val (directed, acted) = getEdges()

    println("Actors Size = " + actors.size)
    println("Movies Size = " + movies.size)

    println("directed Size = " + directed.size)
    println("acted Size = " + acted.size)

    for {
      _ <- insert(directed.map {case JSONDirected(_, from, to) => CompletedRelation(actors(from).toDBPerson, Directed, movies(to).toDBMovie)})
      _ <- insert(acted.map {case JSONActed(_, from, to, _) => CompletedRelation(actors(from).toDBPerson, ActsIn, movies(to).toDBMovie)})

      _ <- insert(actors.collect {case (_, a @ JSONPerson(_, Some(birthday), _, _)) => CompletedRelation(a.toDBPerson, HasBirthday, Date(birthday))})
      _ <- insert(actors.collect {case (_, a @ JSONPerson(_, _, Some(place), _)) => CompletedRelation(a.toDBPerson, BornIn, Place(place))})
      _ <- insert(movies.collect {case (_, m @ JSONMovie(_, _, Some(genre), _)) => CompletedRelation(m.toDBMovie, HasGenre, Genre(genre))})
    } yield ()
  }

  def main(args: Array[String]): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val instance = SQLDB.open(Empty, IMDBSchema.schemaDescription)

    println(instance.fold(e => e,
      implicit instance => Await.result({
        for {
          res <- using(instance) (
            for {
              _ <- buildDB(instance)
              _ = println("Built db")
              bacons <- find(personSchema.pattern("Kevin Bacon".some))
              _ = println("Got bacons")
              res <- bacons.headOption.fold(Operation.point[E, Set[Path[Person]]](Set(), recover))(kb => allShortestPaths(kb, ActsIn --><-- ActsIn))
            } yield res.map{p => (p.length, p.end)}.toList.sortBy(_._1)
          )
        } yield res
      }.run, 12000.seconds)
    ))
  }
}

case class JSONMovie(name: String, language: String, genre: Option[String], key: Int) {
  def dBGenre: Option[Genre] = genre.map(Genre)
  def toDBMovie: Movie = Movie(name, language)
}

object JSONMovie {
  def apply(j: Map[String, JsValue]): JSONMovie = {
    val JsString(name) = j("title")
    val JsString(key) = j("_key")
    val JsString(language) = j("language")
    val genre = j.get("genre").flatMap{ case JsString(g) => Some(g) case _ => None}

    JSONMovie(name, language, genre, key.toInt)
  }
}
case class JSONPerson(name: String, birthday: Option[Long], birthplace: Option[String], key: Int) {
  def toDBPerson: Person = {
    Person(name)
  }

  def dBBirthplace: Option[Place] = birthplace.map(Place)
  def dBBirthday: Option[Date] = birthday.map(Date)
}

object JSONPerson {
  def apply(j: Map[String, JsValue]): JSONPerson = {
    val JsString(name) = j("name")
    val JsString(key) = j("_key")
    val birthday = j.get("birthday").flatMap {case JsString(b) => Some(b.toLong) case _ => None}
    val birthplace = j.get("birthplace").flatMap {case JsString(b) => Some(b) case _ => None}

    JSONPerson(name, birthday, birthplace, key.toInt)
  }
}

case class JSONDirected(key: Int, from: Int, to: Int)
object JSONDirected {
  def apply(m: Map[String, JsValue]): JSONDirected = {
    val JsString(key) = m("_key")
    val JsString(from) = m("_from")
    val JsString(to) = m("_to")

    JSONDirected(key.toInt, from.toInt, to.toInt)

  }
}

case class JSONActed(key: Int, from: Int, to: Int, role: String)
object JSONActed {
  def apply(m: Map[String, JsValue]): JSONActed = {
    val JsString(key) = m("_key")
    val JsString(from) = m("_from")
    val JsString(to) = m("_to")
    val JsString(role) = m("name")

    JSONActed(key.toInt, from.toInt, to.toInt, role)

  }
}