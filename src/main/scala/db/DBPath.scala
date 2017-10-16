package db

/**
  * Created by Al on 07/10/2017.
  */
trait DBPath {

}

object DBPath {
  implicit def fromString(s: String): DBPath = ???
}