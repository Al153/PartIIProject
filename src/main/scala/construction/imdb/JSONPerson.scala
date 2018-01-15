package construction.imdb

import construction.imdb.IMDBSchema.{Date, Person, Place}
import spray.json.{JsString, JsValue}

/**
  * Created by Al on 14/01/2018.
  */
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