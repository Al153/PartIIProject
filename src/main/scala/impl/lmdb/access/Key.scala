package impl.lmdb.access

import java.util.Base64

import core.error.E
import impl.lmdb.errors.EmptyKeyError

import scalaz.Scalaz._
import scalaz._

/**
  * Created by Al on 13/12/2017.
  *
  * Main representation of a key for lmdb
  *
  * Series of non empty strings separated by _
  *
  * Key is appended to a commit ID before storage
  */
class Key private (components: Vector[KeyComponent]) {
  def ++(that: Key) = new Key(this.components ++ that.components)
  def +(that: KeyComponent) = new Key(this.components :+ that)

  def render: String = components.map(_.toBase64).mkString(":")
}

sealed trait KeyComponent {
  def toBase64: String
}

sealed trait Keyable[K] {
 def bytes(k: K): Array[Byte]
}

object Key {
  implicit class KeyableOps[K](k: K)(implicit kev: Keyable[K]) {
    def component = new KeyComponent {
      override def toBase64: String = Base64.getEncoder.encode(kev.bytes(k)).mkString("")
    }

    def key = new Key(Vector(component))
  }
}