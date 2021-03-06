package impl.lmdb.common.access

import java.nio.ByteBuffer
import java.util.Base64

import core.backend.intermediate.RelationName


/**
  * Created by Al on 13/12/2017.
  *
  * Main representation of a key for lmdb
  *
  * Series of non empty byte strings separated by :
  *
  * Key is used to lookup values in the LMDB store
  */
case class Key private (components: Vector[KeyComponent]) {
  /**
    * Append to a key
    */
  def ++(that: Key) = Key(this.components ++ that.components)

  /**
    * Add a value to the key
    */
  def +(that: KeyComponent) = Key(this.components :+ that)

  /**
    * pretty printing
    */
  override def toString: String = components.map(_.toString).toString

  /**
    * render to a base64 key to look up in the LMDB store
    */
  def render(buffer: ByteBuffer): Unit = {
    val bytes = components.map(_.toBase64).mkString(":").getBytes
    buffer.clear()
    buffer.put(bytes).flip()
  }
}

sealed trait KeyComponent {
  /**
    * All key components should be convertable to a base64 string
    * @return
    */
  def toBase64: String
}

/**
  * typeclass for things that can be used as key components
  */

trait Keyable[K] {
  def bytes(k: K): Array[Byte]
}

object Key {

  /**
    * Simple Keyable instance for string
    */
  implicit object KeyableString extends Keyable[String] {
    override def bytes(k: String): Array[Byte] = k.getBytes
  }


  /**
    * Simple Keyable instance for TableName
    */
  implicit object KeyableRelationName extends Keyable[RelationName] {

    /**
      * Hijacks KeyableString's method
      */
    override def bytes(k: RelationName): Array[Byte] = KeyableString.bytes(k.id)
  }

  /**
    * Simple Keyable instance for anything that is Storeable
    */

  implicit def KeyableFromStoreable[A](implicit sa: Storable[A]) = new Keyable[A] {
    /**
      * Hijack Storeable[A]'s toBytes method
      */
    override def bytes(k: A): Array[Byte] = {
      val buf = sa.toBuffer(k)
      val arr = new Array[Byte](buf.remaining())
      buf.get(arr)
      arr
    }
  }


  /**
    * Operations for Keyable objects
    */
  implicit class KeyableOps[K](k: K)(implicit kev: Keyable[K]) {
    /**
      * Convert to a [[KeyComponent]] so the keyable object can be used as a key
      */
    def component = new KeyComponent {
      override def toString: String = k.toString
      override def toBase64: String = Base64.getEncoder.encode(kev.bytes(k)).mkString("")
    }

    /**
      * Convert fully to a key
      */
    def key = new Key(Vector(component))

    /**
      * Key syntax for chaining components
      */
    def >>[A](that: A)(implicit k: Keyable[A]): Key = key ++ that.key
  }


  /**
    * Operations and syntax upon keys
    * @param u - underlying key
    */
  implicit class KeyOps(u: Key) {
    /**
      * Append a key component
      * @param that - the key component to be appended
      */
    def >>[A](that: A)(implicit k: Keyable[A]): Key = u ++ that.key
  }


}