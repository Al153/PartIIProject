package impl

import core.user.containers.ConstrainedFuture
import core.user.dsl.{E, View}
import impl.lmdb.access.{Key, Storeable}
import impl.lmdb.errors.{LMDBError, _}
import org.fusesource.lmdbjni.Database

import scala.concurrent.{ExecutionContext, Promise}
import scalaz.Scalaz._
import scalaz._

/**
  * Created by Al on 28/12/2017.
  */
package object lmdb {

  /**
    * Convenience types
    */
  type LMDBEither[A] = LMDBError \/ A
  type LMDBFuture[A] = ConstrainedFuture[LMDBError, A]


  /**
    * LMDBFuture convenience methods, self explanatory

    */
  def LMDBFutureE[A](ea: LMDBEither[A])(implicit ec: ExecutionContext): LMDBFuture[A] = ConstrainedFuture.either(ea)(recoverLMDBException)
  def LMDBFuture[A](a: => A)(implicit ec: ExecutionContext): LMDBFuture[A] =  ConstrainedFuture.point(a)(recoverLMDBException)

  /**
    * Use a promise to avoid delagating small piece of work to futures
    */
  def LMDBFutureI[A](a: => A)(implicit ec: ExecutionContext): LMDBFuture[A] = ConstrainedFuture.future(Promise.successful(a.right).future)(recoverLMDBException)
  def LMDBFutureER[A](ea: LMDBEither[A])(implicit ec: ExecutionContext): LMDBFuture[A] = ConstrainedFuture.future(Promise.successful(ea).future)(recoverLMDBException)
  def LMDBFailure[A](e: => LMDBError)(implicit ec: ExecutionContext): LMDBFuture[A] = ConstrainedFuture.future(Promise.successful(e.left).future)(recoverLMDBException)
  def LMDBEither[A](a: => A): LMDBEither[A] = try { a.right} catch {case e: Throwable => recoverLMDBException(e).left}
  def safeEither[A](ea: => LMDBEither[A]): LMDBEither[A] = try {ea} catch {case e: Throwable => recoverLMDBException(e).left}
  def LMDBLeft[A](e: => LMDBError): LMDBEither[A] = try {e.left} catch {case e: Throwable => recoverLMDBException(e).left}

  def asE(s: LMDBError): E = s
  def asEither[A](sQLEither: LMDBEither[A]): E \/ A = sQLEither.leftMap(asE)
  def asCFuture[A](f: LMDBFuture[A]): ConstrainedFuture[E, A] = f.leftMap(asE)


  /**
    * Syntax for LMDB Futures
    */
  implicit class LMDBFutureOps[A](u: LMDBFuture[A]) {
    /**
      * Convert and LMDBFuture to an E ConstrainedFuture A
      * @return
      */
    def asCFuture: ConstrainedFuture[E, A] = lmdb.asCFuture(u)
  }

  /**
    * Syntax for LMDB Eithers
    */

  implicit class LMDBEitherOps[A](u: LMDBEither[A]) {
    /**
      * Convert and LMDBEither to an E \/ A
      * @return
      */
    def asEither: E \/ A = lmdb.asEither(u)
  }

  /**
    * Convert a (possible null) array to a vector
    */

  private def safeRetrieve(bytes: Array[Byte]): Vector[Byte]  = Option(bytes).fold(Vector[Byte]()){
    x =>
      println("Vector = " + x.toVector)
      x.toVector
  }

  /**
    * Write a value to a key
    * @param key - key to use
    * @param a the value to write
    * @param sa - storer for type
    * @param db - db (table) to extract from
    * @tparam A - type to extract
    * @return
    */
  def put[A](key: Key, a: A, db: Database)(implicit sa: Storeable[A]): LMDBEither[Unit] = LMDBEither {
    db.put(key.render, sa.toBytes(a).toArray)
  }

  /**
    * Get a value at a key
    * @param key - key to use
    * @param sa - extractor for type
    * @param db - Database(table) to extract from
    * @tparam A - type to extract
    * @return
    */
  def get[A](db: Database, key: Key)(implicit sa: Storeable[A]): LMDBEither[A] =
    for {
      b <- LMDBEither(db.get(key.render))
      r <- sa.fromBytes(safeRetrieve(b))
    } yield r


  /**
    * Gets and puts a new value according to a computation, does this transactionally
    * @param key - key at which to put
    * @param compute - computation to run
    */
  def transactionalGetAndSet[A](key: Key, db: Database)(compute: A => LMDBEither[A])(implicit sa: Storeable[A], instance: LMDBInstance): LMDBEither[A] = {
    import org.fusesource.lmdbjni.Transaction

    // get a new transaction
    val tx: Transaction = instance.env.createWriteTransaction()

    println("tx = " + tx + " id = " + tx.getId)
    // get the key
    val k = key.render
    println("Key = " + key + " Rendered = " + new String(k))
    // instantiate a result value, so there is something if it fails
    var res: LMDBEither[A] = NoResult.left
    try {
        res = for {
          bytes <- LMDBEither(db.get(tx, k))
          _ = println("bytes = " + bytes)
          a <- sa.fromBytes(safeRetrieve(bytes))
          _ = println("a = " + a)
          res <- compute(a)
          _ = println("res = " + res)
          _ <- LMDBEither(db.put(tx, k, sa.toBytes(res).toArray))
        } yield a
        res

    } finally {
      // Make sure you either commit or rollback to avoid resource leaks.
      if (res.isRight) tx.commit()
      else tx.abort()
      tx.close()
    }
  }

  /**
    * Append a value to a set in the LMDB database at a key
    */
  def transactionalAppendToSet[A](key: Key, a: A, db: Database)(implicit sa: Storeable[A], instance: LMDBInstance): LMDBEither[Unit] =
    transactionalGetAndSet[Set[A]](key, db){
      s => (s + a).right
    } map (_ => ())

  /**
    * Union a set of values to the existing one at a key
    */
  def transactionalUnion[A](key: Key, as: Set[A], db: Database)(implicit sa: Storeable[A], instance: LMDBInstance): LMDBEither[Unit] =
    transactionalGetAndSet[Set[A]](key, db) {
      oldAs => LMDBEither(oldAs union as)
    }.map(_ => ())

  // Set up the initial view of the DB
  val initialView = View(0)

}
