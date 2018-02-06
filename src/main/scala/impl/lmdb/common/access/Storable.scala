package impl.lmdb.common.access

import java.nio.ByteBuffer

import core.backend.common._
import core.user.dsl.View
import impl.lmdb.common.LMDBEither
import impl.lmdb.common.access.Storable.StorableInt
import impl.lmdb.common.errors.{BooleanExtractError, UnrecognisedDBHeader}

import scala.collection.mutable
import scalaz.Scalaz._

/**
  * Created by Al on 29/12/2017.
  *
  * Storeable is a typeclass for objects that can be stored in the database
  */
trait Storable[A] {
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
    * Storable objects need to be able to be converted to bytes to be stored
    */
  def writeToBuffer(a: A, buf: ByteBuffer): Unit

  /**
    * Storeable objects need to be extracted from a series of bytes
    * @param buf - buffer to extract from
    * @return
    */
  def fromBuffer(buf: ByteBuffer): LMDBEither[A]
}

/**
  * Storable with a constant length
  * @tparam A - type to store
  */
trait ConstantLengthStorable[A] extends Storable[A] {
  def length: Int
  final override def bufferLength(a: A): Int = length
}

/**
  * Mutable values to write to the database
  * @tparam A - type stored
  * @tparam B - type that can be written
  */
trait MutableStorable[A, B] extends Storable[A] {
  def mutate(b: B, buf: ByteBuffer): ByteBuffer
}

object Storable {

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
    * Extract a buffer into a set of objects using an extractor method
    * @param iterated - a method that picks out values from the start of byte stream and returns the rest
    * @param in - stream to extract from
    * @tparam A - type of objects exracted
    * @return
    */
  def extractWhileList[A](iterated: ByteBuffer => LMDBEither[A], in: ByteBuffer, length: Int): LMDBEither[List[A]] = {
    var res: LMDBEither[mutable.Builder[A, List[A]]] = List.newBuilder[A].right
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
  implicit object StorableView extends ConstantLengthStorable[View] {
    override def fromBuffer(buf: ByteBuffer): LMDBEither[View] = StorableLong.fromBuffer(buf).map(View.apply)

    /**
      * Expected length in buffer
      *
      * @return
      */
    override def length: Int = 8 // length of a long

    /**
      * Storeable objects need to be able to be converted to bytes to be stored
      */
    override def writeToBuffer(a: View, buf: ByteBuffer): Unit = StorableLong.writeToBuffer(a.id, buf)
}

  implicit object StorableString extends Storable[String] {
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

  implicit object StorableBoolean extends ConstantLengthStorable[Boolean] {
    /**``
      * Expected length in buffer
      *
      * @return
      */
    override def length: Int = 1

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
  implicit object StorableInt extends ConstantLengthStorable[Int] {
    /**
      * Expected length in buffer
      *
      * @return
      */
    override def length: Int = 4

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
  implicit object StorableDouble extends Storable[Double] {
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

  implicit object StorableLong extends ConstantLengthStorable[Long] {
    /**
      * Expected length in buffer
      *
      * @return
      */
    override def length: Int = 8

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
  implicit def StoreableSet[A](implicit sa: Storable[A]) = new Storable[Set[A]] {

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
  implicit object StorableDBCell extends Storable[DBCell] {
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
      case DBString(s) => StorableString.bufferLength(s)
      case DBBool(_) => 1
      case DBDouble(_) => 8
    })

    /**
      * Storeable objects need to be able to be converted to bytes to be stored
      */
    override def writeToBuffer(a: DBCell, buf: ByteBuffer): Unit = a match {
      case DBInt(i) => buf.put(intFlag); StorableInt.writeToBuffer(i, buf)
      case DBString(s) =>  buf.put(stringFlag); StorableString.writeToBuffer(s, buf)
      case DBBool(b) =>  buf.put(booleanFlag); StorableBoolean.writeToBuffer(b, buf)
      case DBDouble(d) =>  buf.put(doubleFlag); StorableDouble.writeToBuffer(d, buf)

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
          if (header == intFlag) StorableInt.fromBuffer(buf).map(DBInt)
          else if (header == stringFlag) StorableString.fromBuffer(buf).map(DBString)
          else if (header == doubleFlag) StorableDouble.fromBuffer(buf).map(DBDouble)
          else if (header == booleanFlag) StorableBoolean.fromBuffer(buf).map(DBBool)
          else UnrecognisedDBHeader(header).left
    } yield res
  }

  /**
    * A DBObject is stored as a vector of DBObjects, like a set
    *
    * [Length, Cell, Cell, .., Cell]
    */
  implicit object StorableDBObject extends Storable[DBObject] {
    /**
      * Expected length in buffer
      * int length + each field
      * @return
      */
    override def bufferLength(a: DBObject): Int = 4 + a.fields.foldLeft(0)(_ + StorableDBCell.bufferLength(_))

    /**
      * Storeable objects need to be able to be converted to bytes to be stored
      */
    override def writeToBuffer(a: DBObject, buf: ByteBuffer): Unit = {
      buf.putInt(a.fields.size)
      for {
        f <- a.fields
      } StorableDBCell.writeToBuffer(f, buf)
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
        r <- extractWhileVector(StorableDBCell.fromBuffer, buf, length)
    } yield DBObject(r)
  }

  /**
    * Stored like a set
    */
  implicit def StorableList[A](implicit sa: Storable[A]): Storable[List[A]] = new Storable[List[A]]{
    /**
      * Expected length in buffer
      *
      * @return
      */
    override def bufferLength(a: List[A]): Int = 4 + a.foldLeft(0)(_ + sa.bufferLength(_)) // an int describing the length plus the actual length

    /**
      * Storeable objects need to be able to be converted to bytes to be stored
      */
    override def writeToBuffer(a: List[A], buf: ByteBuffer): Unit = {
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
    override def fromBuffer(buf: ByteBuffer): LMDBEither[List[A]] =
      if (buf.remaining() == 0) List[A]().right
      else for {
        length <- LMDBEither(buf.getInt())
        r <- extractWhileList(sa.fromBuffer, buf, length)
      } yield r
  }





}