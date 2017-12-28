package impl.lmdb.containers

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

  def flatMap[B](f: A => SingleExtractor[B]): Extractor[B]

  def runSet: LMDBFuture[Set[A]]
  def runVector: LMDBFuture[Vector[A]]
}

trait SingleExtractor[A] {
  def map[B](f: A => B): SingleExtractor[B]
  def flatMap[B](f: A => SingleExtractor[B]): SingleExtractor[B]
  def flatMap[B](f: A => Extractor[B]): Extractor[B]

  def run: LMDBFuture[Option[A]]
}
