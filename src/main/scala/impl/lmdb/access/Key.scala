package impl.lmdb.access

import java.util.Base64

import core.schema.{RelationName, TableName}
import org.fusesource.lmdbjni.Constants._


/**
  * Created by Al on 13/12/2017.
  *
  * Main representation of a key for lmdb
  *
  * Series of non empty strings separated by _
  *
  * Key is appended to a commit ID before storage
  */
case class Key private (components: Vector[KeyComponent]) {
  def ++(that: Key) = Key(this.components ++ that.components)
  def +(that: KeyComponent) = Key(this.components :+ that)

  def render: Array[Byte] = bytes(components.map(_.toBase64).mkString(":"))
}

sealed trait KeyComponent {
  def toBase64: String
}

trait Keyable[K] {
 def bytes(k: K): Array[Byte]
}

object Key {

    implicit object KeyableString extends Keyable[String] {

    override def bytes(k: String): Array[Byte] = k.getBytes
  }
  /*
  implicit object KeyableInt extends Keyable[Int] {
    override def bytes(k: Int): Array[Byte] = BigInt(k).toByteArray
  }
  */

implicit object KeyableTableName extends Keyable[TableName] {
  override def bytes(k: TableName): Array[Byte] = KeyableString.bytes(k.value)
}

implicit class KeyableOps[K](k: K)(implicit kev: Keyable[K]) {
  def component = new KeyComponent {
    override def toBase64: String = Base64.getEncoder.encode(kev.bytes(k)).mkString("")
  }

  def key = new Key(Vector(component))
  def >>[A](that: A)(implicit k: Keyable[A]): Key = key :: that.key
}

implicit class KeyComponentOps(u: KeyComponent) {
  def ::(that: KeyComponent): Key = new Key(Vector(that, u))
}

implicit class KeyOps(u: Key) {
  def ::(that: KeyComponent): Key = new Key(that +: u.components)
  def ::(that: Key): Key = that ++ u
  def >>[A](that: A)(implicit k: Keyable[A]): Key = u ++ that.key
}

implicit object KeyableRelationName extends Keyable[RelationName] {
  override def bytes(k: RelationName): Array[Byte] = KeyableString.bytes(k.id)
}

implicit def KeyableFromStoreable[A](implicit sa: Storeable[A]) = new Keyable[A] {
  override def bytes(k: A): Array[Byte] = sa.toBytes(k).toArray
}
}