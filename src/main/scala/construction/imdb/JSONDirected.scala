package construction.imdb

import spray.json.{JsString, JsValue}

/**
  * Created by Al on 14/01/2018.
  */
case class JSONDirected(key: Int, from: Int, to: Int)
object JSONDirected {
  def apply(m: Map[String, JsValue]): JSONDirected = {
    val JsString(key) = m("_key")
    val JsString(from) = m("_from")
    val JsString(to) = m("_to")

    JSONDirected(key.toInt, from.toInt, to.toInt)

  }
}

