package construction.imdb

import spray.json.{JsString, JsValue}

/**
  * Created by Al on 14/01/2018.
  */
case class JSONActed(key: Int, from: Int, to: Int)
object JSONActed {
  def apply(m: Map[String, JsValue]): JSONActed = {
    val JsString(key) = m("_key")
    val JsString(from) = m("_from")
    val JsString(to) = m("_to")

    JSONActed(key.toInt, from.toInt, to.toInt)

  }
}