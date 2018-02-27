package construction.ufc

import java.io.File

import construction.ufc.UFCSchema.{Beat, LighterThan, Person, ShorterThan}
import core.user.containers.Operation
import core.user.dsl.{CompletedRelation, DBDir, E, insert, using}
import core.user.interfaces.DBInstance
import core.utils._
import impl.lmdb.fast.LMDB
import spray.json.{JsArray, JsObject, JsString, _}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.Source

/**
  * Created by Al on 21/01/2018.
  */
object DBBuilder {

  case class JSPerson(name: String, height: Int, weight: Int) {
    def toDBPerson: Person = Person(name, height, weight)
  }

  object JSPerson {
    def apply(m: Map[String, JsValue]): JSPerson = {
      val JsString(name) = m("name")
      val JsNumber(height) = m("height")
      val JsNumber(weight) = m("weight")
      new JSPerson(name, height.toInt, weight.toInt)
    }


  }
  case class JSShorter(from: JSPerson, to: JSPerson)
  object JSShorter {
    def apply(m: Map[String, JsValue]): JSShorter = {
      val JsObject(from) = m("shorter")
      val JsObject(to) = m("taller")
      JSShorter(JSPerson(from), JSPerson(to))
    }
  }

  case class JSLighter(from: JSPerson, to: JSPerson)
  object JSLighter {
    def apply(m: Map[String, JsValue]): JSLighter = {
      val JsObject(from) = m("lighter")
      val JsObject(to) = m("heavier")
      JSLighter(JSPerson(from), JSPerson(to))
    }
  }

  case class JSBeat(from: JSPerson, to: JSPerson)
  object JSBeat {
    def apply(m: Map[String, JsValue]): JSBeat = {
      val JsObject(from) = m("winner")
      val JsObject(to) = m("loser")
      JSBeat(JSPerson(from), JSPerson(to))
    }
  }

  def buildDB[E1 <: E](sourcePath: String)(implicit instance: DBInstance[E1]): Operation[E1, Unit] = {

    def getEdges(sourcePath: String): (Set[JSShorter], Set[JSLighter], Set[JSBeat]) = {
      val allObjects = Source.fromResource(s"$sourcePath/edges.json").mkString.parseJson
      val JsArray(objects) = allObjects

      val shorter = objects.collect {case JsObject(d) if ("label" in d) && (d("label") == JsString("ShorterThan")) => JSShorter(d)}.toSet
      val lighter = objects.collect {case JsObject(a) if ("label" in a) && (a("label") == JsString("LighterThan")) => JSLighter(a)}.toSet
      val beat = objects.collect {case JsObject(a) if ("label" in a) && (a("label") == JsString("Beat")) => JSBeat(a)}.toSet
      (shorter, lighter, beat)
    }

    val (shorter, lighter, beat) = getEdges(sourcePath)

    println("shorter Size = " + shorter.size)
    println("Lighter Size = " + lighter.size)
    println("Beat Size = " + beat.size)

    for {
      _ <- insert(shorter.collect {case JSShorter(a, b) => CompletedRelation(a.toDBPerson, ShorterThan, b.toDBPerson)})
      _ <- insert(lighter.collect {case JSLighter(a, b) => CompletedRelation(a.toDBPerson, LighterThan, b.toDBPerson)})
      _ <- insert(beat.collect {case JSBeat(a, b) => CompletedRelation(a.toDBPerson, Beat, b.toDBPerson)})
    } yield ()
  }

  def main(args: Array[String]): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val testLocation = if (System.getProperty("os.name") == "Linux") {
      "/home/at736/dev/part_2_db"
    } else {
      "/dev/part_2_db"
    }
    val testName = "ufc"

    val instance = LMDB.open(DBDir(new File(s"$testLocation/$testName").toPath, "", ""), UFCSchema.schema)
    // val instance = LMDB.open(Empty, IMDBSchema.schemaDescription)
    // val instance = SQLDB.open(Empty, IMDBSchema.schemaDescription)
    // val instance = MemoryDB.open(Empty, IMDBSchema.schemaDescription)

    println(instance.fold(e => e,
      implicit instance => Await.result({
        for {
          res <- using(instance) (
            for {
              _ <- buildDB(sourcePath = testName)(instance)
              _ = println("Built db")
            } yield  ()
          )
        } yield res
      }.run, 120000.seconds)
    ))
  }
}
