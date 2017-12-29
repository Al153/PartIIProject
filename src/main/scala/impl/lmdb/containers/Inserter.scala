package impl.lmdb.containers

import impl.sql.SQLFuture

/**
  * Created by Al on 29/12/2017.
  */
trait Inserter[A] {
  def insertSingle(a: A): Inserter[A]
  def insertFromSingleExtracter(sa: SingleExtractor[A]): Inserter[A]
  def insert(as: TraversableOnce[A]): Inserter[A]
  def insertFromExtracter(ea: Extractor[A]): Inserter[A]

  def run: SQLFuture[Unit]
}
