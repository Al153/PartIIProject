package impl.lmdb.access

import java.nio.ByteBuffer

import core.backend.common._
import core.view.View
import impl.lmdb.LMDBEither
import impl.lmdb.errors.{BooleanExtractError, UnexpectedStreamLength, UnrecognisedDBHeader}

import scalaz.Scalaz._
import scalaz._

/**
  * Created by Al on 29/12/2017.
  */
trait Storeable[A] {
  def toBytes(a: A): Vector[Byte]

  def fromBytes(bytes: Vector[Byte]): LMDBEither[A]
}

object Storeable {
  def extractWhileSet[A](iterated: Vector[Byte] => LMDBEither[(A, Vector[Byte])])
                        (in: Vector[Byte]): LMDBEither[Set[A]] = {
    var res: LMDBEither[Set[A]] = Set[A]().right
    var arr = in
    while (arr.nonEmpty && res.isRight) {
      println("\t\t\tarray = " + arr)
      res = for {
        aAndRest <- iterated(arr)
        (a, rest) = aAndRest
        as <- res
      } yield {arr = rest; println("\t\t\t\tarray = " + arr) ; as + a}
    }
    res
  }

  def extractWhileVector[A](iterated: Vector[Byte] => LMDBEither[(A, Vector[Byte])])
                        (in: Vector[Byte]): LMDBEither[Vector[A]] = {
    var res: LMDBEither[Vector[A]] = Vector[A]().right
    var arr = in
    while (arr.nonEmpty && res.isRight) {
      res = for {
        aAndRest <- iterated(arr)
        (a, rest) = aAndRest
        as <- res
      } yield {arr = rest; as :+ a}
    }
    res
  }


  implicit object StoreableView extends Storeable[View] {
    override def toBytes(v: View): Vector[Byte] = StoreableLong.toBytes(v.id)
    override def fromBytes(bytes: Vector[Byte]): LMDBEither[View] = StoreableLong.fromBytes(bytes).map(View.apply)
  }

  implicit object StoreableString extends Storeable[String] {
    override def toBytes(s: String): Vector[Byte] = {
     s.getBytes.toVector
    }

    override def fromBytes(bytes: Vector[Byte]): LMDBEither[String] =
      new String(bytes.toArray).right

  }

  implicit object StoreableBoolean extends Storeable[Boolean] {
    override def toBytes(a: Boolean): Vector[Byte] =
      if (a) Vector(255.toByte) else Vector(0)

    override def fromBytes(bytes: Vector[Byte]): LMDBEither[Boolean] =
      if (bytes == Vector(255.toByte)) true.right
      else if (bytes == Vector(0.toByte)) false.right
      else BooleanExtractError(bytes).left
  }

  implicit object StoreableInt extends Storeable[Int] {
    override def toBytes(a: Int): Vector[Byte] =
      Vector((a>>24).toByte, (a>>16).toByte, (a>>8).toByte, a.toByte)

    override def fromBytes(bytes: Vector[Byte]): LMDBEither[Int] =
      LMDBEither(bytes.foldLeft[Int](0) {
        case (top, b) => (top << 8) + b
      })
  }

  implicit object StoreableDouble extends Storeable[Double] {
    override def toBytes(d: Double): Vector[Byte] = {
      val l = java.lang.Double.doubleToLongBits(d)
      val a = Array.fill(8)(0.toByte)
      for (i <- 0 to 7) a(i) = ((l >> ((7 - i) * 8)) & 0xff).toByte
      a.toVector
    }

    override def fromBytes(bytes: Vector[Byte]): LMDBEither[Double] =
      if (bytes.length == 8) ByteBuffer.wrap(bytes.toArray).getDouble.right
      else UnexpectedStreamLength(8, bytes).left
  }

  implicit object StoreableLong extends Storeable[Long] {
    override def toBytes(a: Long): Vector[Byte] =
      Vector(
        (a>>56).toByte, (a>>48).toByte, (a>>40).toByte, (a>>32).toByte,
        (a>>24).toByte, (a>>16).toByte, (a>> 8).toByte, a.toByte
      )

    override def fromBytes(bytes: Vector[Byte]): LMDBEither[Long] =
      LMDBEither(bytes.foldLeft[Long](0) {
        case (top, b) => (top << 8) + b
      })
  }

  implicit def StoreableSet[A](implicit sa: Storeable[A]) = new Storeable[Set[A]] {
    override def toBytes(s: Set[A]): Vector[Byte] = s.foldRight(Vector[Byte]()) {
      case (a, rest) =>
        val bytes = sa.toBytes(a)
        StoreableInt.toBytes(bytes.length) ++ bytes ++ rest
    }

    override def fromBytes(bytes: Vector[Byte]): LMDBEither[Set[A]] = {
      println("Extracting set " + bytes)
      def iterator(in: Vector[Byte]): LMDBEither[(A, Vector[Byte])] = for {
          count <- StoreableInt.fromBytes(in.take(4))
          tail = in.drop(4)
          topAndTail <-
            if (tail.length >= count) tail.splitAt(count).right
            else UnexpectedStreamLength(count, tail).left

          (top, tail) = topAndTail
          a <- sa.fromBytes(top)
        } yield (a, tail)

      val res = extractWhileSet(iterator)(bytes)
      println("\t\tgot: " + res)
      res
    }
  }

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