package impl.lmdb.access

import java.nio.ByteBuffer

import core.backend.common._
import core.user.dsl.View
import impl.lmdb.LMDBEither
import impl.lmdb.errors.{BooleanExtractError, UnexpectedStreamLength, UnrecognisedDBHeader}

import scala.collection.mutable
import scalaz.Scalaz._

/**
  * Created by Al on 29/12/2017.
  *
  * Storeable is a typeclass for objects that can be stored in the database
  */
trait Storeable[A] {
  /**
    * Storeable objects need to be able to be converted to bytes to be stored
    */
  def toBytes(a: A): Vector[Byte]

  /**
    * Storeable objects need to be extracted from a series of bytes
    * @param bytes
    * @return
    */
  def fromBytes(bytes: Vector[Byte]): LMDBEither[A]
}

object Storeable {

  // todo: should this use arrays underneath?
  // todo: optimise everything
  /**
    * Extract a vector of bytes into a set of objects using an extractor method
    * @param iterated - a method that picks out values from the start of byte stream and returns the rest
    * @param in - stream to extract from
    * @tparam A - type of objects exracted
    * @return
    */
  def extractWhileSet[A](iterated: Vector[Byte] => LMDBEither[(A, Vector[Byte])])
                        (in: Vector[Byte]): LMDBEither[Set[A]] = {
    var res: LMDBEither[mutable.Builder[A, Set[A]]] = Set.newBuilder[A].right
    var arr = in
    while (arr.nonEmpty && res.isRight) {
      res = for {
        aAndRest <- iterated(arr)
        (a, rest) = aAndRest
        as <- res
      } yield {arr = rest; as.+=(a)}
    }
    res.map(_.result())
  }

  /**
    * Extract a vector of bytes into a vector of objects using an extractor method
    * @param iterated - a method that picks out values from the start of byte stream and returns the rest
    * @param in - stream to extract from
    * @tparam A - type of objects exracted
    * @return
    */
  def extractWhileVector[A](iterated: Vector[Byte] => LMDBEither[(A, Vector[Byte])])
                        (in: Vector[Byte]): LMDBEither[Vector[A]] = {
    var res: LMDBEither[mutable.Builder[A, Vector[A]]] = Vector.newBuilder[A].right
    var arr = in
    while (arr.nonEmpty && res.isRight) {
      res = for {
        aAndRest <- iterated(arr)
        (a, rest) = aAndRest
        as <- res
      } yield {arr = rest; as += a}
    }
    res.map(_.result())
  }

  /**
    * Storeable instances for common classes
    */
  implicit object StoreableView extends Storeable[View] {
    override def toBytes(v: View): Vector[Byte] = StoreableLong.toBytes(v.id)
    override def fromBytes(bytes: Vector[Byte]): LMDBEither[View] = StoreableLong.fromBytes(bytes).map(View.apply)
  }

  implicit object StoreableString extends Storeable[String] {
    /**
      * String conversions are easy
      */
    override def toBytes(s: String): Vector[Byte] = {
     s.getBytes.toVector
    }

    override def fromBytes(bytes: Vector[Byte]): LMDBEither[String] =
      new String(bytes.toArray).right

  }

  /**
    * A boolean is stored as either a 0xff or 0x00
    */

  implicit object StoreableBoolean extends Storeable[Boolean] {
    override def toBytes(a: Boolean): Vector[Byte] =
      if (a) Vector(255.toByte) else Vector(0)

    override def fromBytes(bytes: Vector[Byte]): LMDBEither[Boolean] =
      if (bytes == Vector(255.toByte)) true.right
      else if (bytes == Vector(0.toByte)) false.right
      else BooleanExtractError(bytes).left
  }

  /**
    * An int is always stored as 4 bytes in big-endian format
    */
  implicit object StoreableInt extends Storeable[Int] {
    override def toBytes(a: Int): Vector[Byte] =
      Vector((a>>24).toByte, (a>>16).toByte, (a>>8).toByte, a.toByte)

    override def fromBytes(bytes: Vector[Byte]): LMDBEither[Int] =
      LMDBEither(bytes.foldLeft[Int](0) {
        case (top, b) => (top << 8) + b
      })
  }

  /**
    * Doubles are stored in big endian format (8 bytes)
    */
  implicit object StoreableDouble extends Storeable[Double] {
    override def toBytes(d: Double): Vector[Byte] = {
      val l = java.lang.Double.doubleToLongBits(d)
      StoreableLong.toBytes(l)
    }

    override def fromBytes(bytes: Vector[Byte]): LMDBEither[Double] =
      for (l <- StoreableLong.fromBytes(bytes))
        yield java.lang.Double.longBitsToDouble(l)
  }

  /**
    * Longs are also stored in big endian format
    */

  implicit object StoreableLong extends Storeable[Long] {
    override def toBytes(a: Long): Vector[Byte] = {
      var l = a
      val result = new Array[Byte](8)
      var i = 7
      while (i >= 0) {
        result(i) = (l & 0xFF).toByte
        l >>= 8
        i -= 1
      }
      result.toVector
    }

    override def fromBytes(bytes: Vector[Byte]): LMDBEither[Long] = {
      var result = 0l
      for (b <- bytes) {
        result <<= 8
        result |= (b & 0xFF)
      }
      result.right
    }
  }

  /**
    * Sets are stored as [length - object - length - object - ... object]
    * With length as a 4 byte int
    * @return
    */
  implicit def StoreableSet[A](implicit sa: Storeable[A]) = new Storeable[Set[A]] {
    override def toBytes(s: Set[A]): Vector[Byte] = s.foldRight(Vector[Byte]()) {
      case (a, rest) =>
        val bytes = sa.toBytes(a)
        StoreableInt.toBytes(bytes.length) ++ bytes ++ rest
    }

    override def fromBytes(bytes: Vector[Byte]): LMDBEither[Set[A]] = {
      def iterator(in: Vector[Byte]): LMDBEither[(A, Vector[Byte])] = for {
          count <- StoreableInt.fromBytes(in.take(4))
          tail = in.drop(4)
          topAndTail <-
            if (tail.length >= count) tail.splitAt(count).right
            else UnexpectedStreamLength(count, tail).left

          (top, tail) = topAndTail
          a <- sa.fromBytes(top)
        } yield (a, tail)

      extractWhileSet(iterator)(bytes)
    }
  }

  /**
    * A DBCell is stored as a flag-byte followed by the representation of a value
    */
  implicit object StoreableDBCell extends Storeable[DBCell] {
    val intFlag: Byte = 0.toByte
    val stringFlag: Byte = 1.toByte
    val doubleFlag: Byte = 2.toByte
    val booleanFlag: Byte = 3.toByte

    override def toBytes(a: DBCell): Vector[Byte] = a match {
      case DBInt(i) => intFlag +: StoreableInt.toBytes(i)
      case DBString(s) => stringFlag +: StoreableString.toBytes(s)
      case DBBool(b) => booleanFlag +: StoreableBoolean.toBytes(b)
      case DBDouble(d) => doubleFlag +: StoreableDouble.toBytes(d)

    }
    override def fromBytes(bytes: Vector[Byte]): LMDBEither[DBCell] =
      if (bytes.nonEmpty) {
        val header = bytes.head
        val rest = bytes.tail
        if (header == intFlag) StoreableInt.fromBytes(rest).map(DBInt)
        else if (header == stringFlag) StoreableString.fromBytes(rest).map(DBString)
        else if (header == doubleFlag) StoreableDouble.fromBytes(rest).map(DBDouble)
        else if (header == booleanFlag) StoreableBoolean.fromBytes(rest).map(DBBool)
        else UnrecognisedDBHeader(header).left
      } else UnexpectedStreamLength(1, bytes).left
  }

  /**
    * A DBObject is stored as a vector of DBObjects, like a set
    *
    * [Length, Cell, Length, Cell, .., Cell]
    */
  implicit object StoreableDBObject extends Storeable[DBObject] {
    override def toBytes(o: DBObject): Vector[Byte] = o.fields.foldRight(Vector[Byte]()) {
      case (a, rest) =>
        val bytes = StoreableDBCell.toBytes(a)
        StoreableInt.toBytes(bytes.length) ++ bytes ++ rest
    }

    override def fromBytes(bytes: Vector[Byte]): LMDBEither[DBObject] = {
      def iterator(in: Vector[Byte]): LMDBEither[(DBCell, Vector[Byte])] = for {
        count <- StoreableInt.fromBytes(in.take(4))
        tail = in.drop(4)
        topAndTail <-
         if (tail.length >= count) tail.splitAt(count).right
          else UnexpectedStreamLength(count, tail).left
        (top, tail) = topAndTail
        a <- StoreableDBCell.fromBytes(top)
      } yield (a, tail)

      for {
        fields <- extractWhileVector(iterator)(bytes)
      } yield DBObject(fields)
    }
  }
}