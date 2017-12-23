package core.utils

import java.util.concurrent.ConcurrentHashMap
import scala.collection.JavaConverters._

/**
  * Created by Al on 23/12/2017.
  */
class Memo[A, B](f: A => B) {
  private val m = new ConcurrentHashMap[A, B]().asScala

  def apply(a: A): B = this.synchronized {
    m.getOrElse(a, {
      val b = f(a); m(a) = b; b
    })
  }
}
