package construction.imdb

import construction.imdb.IMDBSchema.{Genre, Movie}
import spray.json.{JsString, JsValue}

/**
  * Created by Al on 14/01/2018.
  */

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


