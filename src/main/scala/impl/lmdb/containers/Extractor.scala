package impl.lmdb.containers

import impl.lmdb._
import impl.lmdb.errors.LMDBError

import scala.collection.SeqLike

/**
  * Created by Al on 28/12/2017.
  *
  * Interface for objects to be used to extract collections of values from the database
  */
trait Extractor[A] {
  def map[B](f: A => B): Extractor[B]
  def flatMap[B](f: A => Extractor[B]): Extractor[B]
  def filter(p: A => Boolean): Extractor[A]
  def collect[B](pf: PartialFunction[A, B]): Extractor[B]

  def flatExtractor[B](f: A => SingleExtractor[B]): Extractor[B]
  def union(that: Extractor[A]): Extractor[A]
  def intersection(that: Extractor[A]): Extractor[A]

  def runSet: LMDBFuture[Set[A]]
  def runVector: LMDBFuture[Vector[A]]
}

trait SingleExtractor[A] {
  def map[B](f: A => B): SingleExtractor[B]
  def flatMap[B](f: A => SingleExtractor[B]): SingleExtractor[B]
  def flatMap[B](f: A => Extractor[B]): Extractor[B]

  def run: LMDBFuture[Option[A]]
}



// empty extractor:
case class EmptyExtractor[A]() extends Extractor[A] {
  override def map[B](f: (A) => B): Extractor[B] = EmptyExtractor[B]()

  override def flatMap[B](f: (A) => Extractor[B]): Extractor[B] = EmptyExtractor[B]()

  override def filter(p: (A) => Boolean): Extractor[A] = EmptyExtractor[A]()

  override def collect[B](pf: PartialFunction[A, B]): Extractor[B] = EmptyExtractor[B]()

  override def flatExtractor[B](f: (A) => SingleExtractor[B]): Extractor[B] = EmptyExtractor[B]()

  override def union(that: Extractor[A]): Extractor[A] = that // identity under union

  override def intersection(that: Extractor[A]): Extractor[A] = this // destructor under intersection

  override def runSet: LMDBFuture[Set[A]] = LMDBFuture(Set()) // is empty
  override def runVector: LMDBFuture[Vector[A]] = LMDBFuture(Vector()) // is empty
}

// extractor that has failed
case class ErrorExtractor[A](e: LMDBError) extends Extractor[A] {
  override def map[B](f: (A) => B): Extractor[B] = ErrorExtractor(e)

  override def flatMap[B](f: (A) => Extractor[B]): Extractor[B] = ErrorExtractor(e)

  override def filter(p: (A) => Boolean): Extractor[A] = ErrorExtractor(e)

  override def collect[B](pf: PartialFunction[A, B]): Extractor[B] = ErrorExtractor(e)

  override def flatExtractor[B](f: (A) => SingleExtractor[B]): Extractor[B] = ErrorExtractor(e)

  override def union(that: Extractor[A]): Extractor[A] = ErrorExtractor(e)

  override def intersection(that: Extractor[A]): Extractor[A] = ErrorExtractor(e)

  override def runSet: LMDBFuture[Set[A]] = LMDBFailure[Set[A]](e)

  override def runVector: LMDBFuture[Vector[A]] = LMDBFailure[Vector[A]](e)
}



object Extractor {
  def bigUnion[A](te: TraversableOnce[Extractor[A]]): Extractor[A] = te.foldLeft(EmptyExtractor[A](): Extractor[A]) { _ union _}
  def bigIntersection[A](ifEmpty: Extractor[A])(set: SeqLike[Extractor[A], _]): Extractor[A] = set match {
    case first +: rest => rest.foldLeft(first){_ intersection _}
    case _ => ifEmpty
  }
}