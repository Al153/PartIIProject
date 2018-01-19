package impl.lmdbfast.access

import java.nio.ByteBuffer

import core.backend.common._
import core.user.dsl.View
import impl.lmdbfast.LMDBEither
import impl.lmdbfast.errors.{BooleanExtractError, UnrecognisedDBHeader}

import scala.collection.mutable
import scalaz.Scalaz._

/**
  * Created by Al on 29/12/2017.
  *
  * Storeable is a typeclass for objects that can be stored in the database
  */
trait Storeable[A] {
  /**
    * Expected length in buffer
    * @return
    */
  def bufferLength(a: A): Int

  /**
    * Top level fn to be called to get a byte buffer
    * @param a - object to write
    * @return
    */
  final def toBuffer(a: A): ByteBuffer = {
    val buf = ByteBuffer.allocateDirect(bufferLength(a))
    writeToBuffer(a, buf)
    buf.flip()
    buf
  }

  /**
    * Storeable objects need to be able to be converted to bytes to be stored
    */
  def writeToBuffer(a: A, buf: ByteBuffer): Unit

  /**
    * Storeable objects need to be extracted from a series of bytes
    * @param buf - buffer to extract from
    * @return
    */
  def fromBuffer(buf: ByteBuffer): LMDBEither[A]
}

object Storeable {

  /**
    * Extract a buffer into a set of objects using an extractor method
    * @param iterated - a method that picks out values from the start of byte stream and returns the rest
    * @param in - stream to extract from
    * @tparam A - type of objects exracted
    * @return
    */
  def extractWhileSet[A](iterated: ByteBuffer => LMDBEither[A], in: ByteBuffer, length: Int): LMDBEither[Set[A]] = {
    var res: LMDBEither[mutable.Builder[A, Set[A]]] = Set.newBuilder[A].right
    var i = 0
    while (in.remaining() > 0 && res.isRight && i < length) {
      i += 1
      res = for {
        a <- iterated(in)
        as <- res
      } yield as.+=(a)
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
  def extractWhileVector[A](iterated: ByteBuffer => LMDBEither[A], in: ByteBuffer, length: Int): LMDBEither[Vector[A]] = {
    var res: LMDBEither[mutable.Builder[A, Vector[A]]] = Vector.newBuilder[A].right
    var i = 0
    while (in.remaining() > 0 && res.isRight && i < length) {
      i += 1
      res = for {
        a <- iterated(in)
        as <- res
      } yield as.+=(a)
    }
    res.map(_.result())
  }

  /**
    * Storeable instances for common classes
    */
  implicit object StoreableView extends Storeable[View] {
    override def fromBuffer(buf: ByteBuffer): LMDBEither[View] = StoreableLong.fromBuffer(buf).map(View.apply)

    /**
      * Expected length in buffer
      *
      * @return
      */
    override def bufferLength(a: View): Int = 8 // length of a long

    /**
      * Storeable objects need to be able to be converted to bytes to be stored
      */
    override def writeToBuffer(a: View, buf: ByteBuffer): Unit = StoreableLong.writeToBuffer(a.id, buf)
}

  implicit object StoreableString extends Storeable[String] {
    override def bufferLength(a: String): Int = a.getBytes().length + 4 // a char is 2 bytes, plus 4 bytes for an int
    /**
      * String conversions are easy
      */
    /**
      * Storeable objects need to be able to be converted to bytes to be stored
      */
    override def writeToBuffer(a: String, buf: ByteBuffer): Unit = {
      val bytes = a.getBytes()
      buf.putInt(bytes.length)
      buf.put(bytes)
    }

    /**
      * Storeable objects need to be extracted from a series of bytes
      *
      * @param buf - buffer to extract from
      * @return
      */
    override def fromBuffer(buf: ByteBuffer): LMDBEither[String] = LMDBEither {
      val length = buf.getInt
      val bytes = new Array[Byte](length)
      buf.get(bytes)
      new String(bytes)
    }
  }

  /**
    * A boolean is stored as either a 0xff or 0x00
    */

  implicit object StoreableBoolean extends Storeable[Boolean] {
    /**``
      * Expected length in buffer
      *
      * @return
      */
    override def bufferLength(a: Boolean): Int = 1

    /**
      * Storeable objects need to be able to be converted to bytes to be stored
      */
override def writeToBuffer(a: Boolean, buf: ByteBuffer): Unit =
  if (a){
    buf.put((-1).toByte)
  } else {
    buf.put(0.toByte)
  }

    /**
      * Storeable objects need to be extracted from a series of bytes
      *
      * @param buf - buffer to extract from
      * @return
      */
    override def fromBuffer(buf: ByteBuffer): LMDBEither[Boolean] =
      for  {
        b <- LMDBEither(buf.get())
        r <- if (b == -1) true.right
        else if (b == 0) false.right
        else BooleanExtractError(b).left
      } yield r
}

  /**
    * An int is always stored as 4 bytes in big-endian format
    */
  implicit object StoreableInt extends Storeable[Int] {
    /**
      * Expected length in buffer
      *
      * @return
      */
    override def bufferLength(a: Int): Int = 4

    /**
      * Storeable objects need to be able to be converted to bytes to be stored
      */
    override def writeToBuffer(a: Int, buf: ByteBuffer): Unit = buf.putInt(a)

    /**
      * Storeable objects need to be extracted from a series of bytes
      *
      * @param buf - buffer to extract from
      * @return
      */
    override def fromBuffer(buf: ByteBuffer): LMDBEither[Int] = LMDBEither(buf.getInt())
}

  /**
    * Doubles are stored in big endian format (8 bytes)
    */
  implicit object StoreableDouble extends Storeable[Double] {
    /**
      * Expected length in buffer
      *
      * @return
      */
    override def bufferLength(a: Double): Int = 8

    /**
      * Storeable objects need to be able to be converted to bytes to be stored
      */
    override def writeToBuffer(a: Double, buf: ByteBuffer): Unit = buf.putDouble(a)

    /**
      * Storeable objects need to be extracted from a series of bytes
      *
      * @param buf - buffer to extract from
      * @return
      */
    override def fromBuffer(buf: ByteBuffer): LMDBEither[Double] = LMDBEither(buf.getDouble())
}

  /**
    * Longs are also stored in big endian format
    */

  implicit object StoreableLong extends Storeable[Long] {
    /**
      * Expected length in buffer
      *
      * @return
      */
    override def bufferLength(a: Long): Int = 8

    /**
      * Storeable objects need to be able to be converted to bytes to be stored
      */
    override def writeToBuffer(a: Long, buf: ByteBuffer): Unit = buf.putLong(a)

    /**
      * Storeable objects need to be extracted from a series of bytes
      *
      * @param buf - buffer to extract from
      * @return
      */
    override def fromBuffer(buf: ByteBuffer): LMDBEither[Long] = {
      LMDBEither(buf.getLong())
    }
}

  /**
    * Sets are stored as [length - object - object - ... object]
    * With length as a 4 byte int
    * @return
    */
  implicit def StoreableSet[A](implicit sa: Storeable[A]) = new Storeable[Set[A]] {
    /**
      * Expected length in buffer
      *
      * @return
      */
    override def bufferLength(a: Set[A]): Int = 4 + a.foldLeft(0)(_ + sa.bufferLength(_)) // an int describing the length plus the actual length

    /**
      * Storeable objects need to be able to be converted to bytes to be stored
      */
    override def writeToBuffer(a: Set[A], buf: ByteBuffer): Unit = {
      buf.putInt(a.size)
      for (a <- a) {
        sa.writeToBuffer(a, buf)
      }
    }

    /**
      * Storeable objects need to be extracted from a series of bytes
      *
      * @param buf - buffer to extract from
      * @return
      */
    override def fromBuffer(buf: ByteBuffer): LMDBEither[Set[A]] =
      if (buf.remaining() == 0) Set[A]().right
      else for {
        length <- LMDBEither(buf.getInt())
        r <- extractWhileSet(sa.fromBuffer, buf, length)
      } yield r
}

  /**
    * A DBCell is stored as a flag-byte followed by the representation of a value
    */
  implicit object StoreableDBCell extends Storeable[DBCell] {
    val intFlag: Byte = 0.toByte
    val stringFlag: Byte = 1.toByte
    val doubleFlag: Byte = 2.toByte
    val booleanFlag: Byte = 3.toByte

    /**
      * Expected length in buffer
      *
      * @return
      */
    override def bufferLength(a: DBCell): Int = 1 + (a match {
      case DBInt(_) => 4
      case DBString(s) => StoreableString.bufferLength(s)
      case DBBool(_) => 1
      case DBDouble(_) => 8
    })

    /**
      * Storeable objects need to be able to be converted to bytes to be stored
      */
    override def writeToBuffer(a: DBCell, buf: ByteBuffer): Unit = a match {
      case DBInt(i) => buf.put(intFlag); StoreableInt.writeToBuffer(i, buf)
      case DBString(s) =>  buf.put(stringFlag); StoreableString.writeToBuffer(s, buf)
      case DBBool(b) =>  buf.put(booleanFlag); StoreableBoolean.writeToBuffer(b, buf)
      case DBDouble(d) =>  buf.put(doubleFlag); StoreableDouble.writeToBuffer(d, buf)

    }

    /**
      * Storeable objects need to be extracted from a series of bytes
      *
      * @param buf - buffer to extract from
      * @return
      */
    override def fromBuffer(buf: ByteBuffer): LMDBEither[DBCell] =
      for {
        header <- LMDBEither(buf.get())
        res <-
          if (header == intFlag) StoreableInt.fromBuffer(buf).map(DBInt)
          else if (header == stringFlag) StoreableString.fromBuffer(buf).map(DBString)
          else if (header == doubleFlag) StoreableDouble.fromBuffer(buf).map(DBDouble)
          else if (header == booleanFlag) StoreableBoolean.fromBuffer(buf).map(DBBool)
          else UnrecognisedDBHeader(header).left
    } yield res
}

  /**
    * A DBObject is stored as a vector of DBObjects, like a set
    *
    * [Length, Cell, Cell, .., Cell]
    */
  implicit object StoreableDBObject extends Storeable[DBObject] {
    /**
      * Expected length in buffer
      * int length + each field
      * @return
      */
    override def bufferLength(a: DBObject): Int = 4 + a.fields.foldLeft(0)(_ + StoreableDBCell.bufferLength(_))

    /**
      * Storeable objects need to be able to be converted to bytes to be stored
      */
    override def writeToBuffer(a: DBObject, buf: ByteBuffer): Unit = {
      buf.putInt(a.fields.size)
      for {
        f <- a.fields
      } StoreableDBCell.writeToBuffer(f, buf)
    }

    /**
      * Storeable objects need to be extracted from a series of bytes
      *
      * @param buf - buffer to extract from
      * @return
      */
    override def fromBuffer(buf: ByteBuffer): LMDBEither[DBObject] =
      for {
        length <- LMDBEither(buf.getInt())
        r <- extractWhileVector(StoreableDBCell.fromBuffer, buf, length)
    } yield DBObject(r)
  }
}