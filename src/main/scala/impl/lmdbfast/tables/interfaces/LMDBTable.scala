package impl.lmdbfast.tables.interfaces

import java.nio.ByteBuffer

import impl.lmdbfast.access.{Key, Storable}
import impl.lmdbfast.errors.NoResult
import impl.lmdbfast.{LMDBEither, LMDBInstance}
import org.lmdbjava.Dbi

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scalaz.Scalaz._
import scalaz._
/**
  * Created by Al on 28/12/2017.
  *
  * An LMDB table is a namespace inside the flat LMDB structure
  */
trait LMDBTable {
  def name: String
  implicit val instance: LMDBInstance
  def db: Dbi[ByteBuffer]
  def initialise(): LMDBEither[Unit]

  /**
    * Holds the keyBuffer
    */
  object KeyBuffer {
    // todo: limit keys to 512 by limiting string lengths
    private val buf = ByteBuffer.allocateDirect(511)
    def withBuf[A](f: ByteBuffer => A): A = this.synchronized(f(buf))
  }

  /**
    * Write a value to a key
    * @param key - key to use
    * @param a the value to write
    * @param sa - storer for type

    * @tparam A - type to extract
    * @return
    */
  def put[A](key: Key, a: A)(implicit sa: Storable[A], instance: LMDBInstance): LMDBEither[Unit] = LMDBEither {
    KeyBuffer.withBuf{
      keyBuf =>
        key.render(keyBuf)
        val valueBuf = sa.toBuffer(a)
        db.put(keyBuf, valueBuf)
    }
  }

  def safeExtract(buf: ByteBuffer): ByteBuffer = if (null == buf) ByteBuffer.allocateDirect(0) else buf

  /**
    * Get a value at a key
    *
    * @param key - key to use
    * @param sa - extractor for type
    * @tparam A - type to extract
    * @return
    */
  def get[A](key: Key)(implicit instance: LMDBInstance, sa: Storable[A]): LMDBEither[A] = {
    val tx = instance.env.txnRead()

    for {
      buf <- LMDBEither(try {
        KeyBuffer.withBuf{ keyBuf =>
          key.render(keyBuf)
          db.get(tx, keyBuf)
        }

      } finally {
        if (tx != null) tx.close()
      })
      res <- sa.fromBuffer(safeExtract(buf))
    } yield res
  }

  /**
    * Batch a set of transactions under one transaction
    * @param keys - keys to read from
    * @param instance - instance to execute against - todo: is this needed?
    * @param sa - extractor for A
    * @tparam A - objects exprected
    * @return
    */
  def getBatch[A,  M[X] <: TraversableOnce[X]](keys: M[Key])(implicit instance: LMDBInstance, sa: Storable[A], cbf: CanBuildFrom[M[Key], A, M[A]]): LMDBEither[M[A]] = {
    val tx = instance.env.txnRead()
    // catch any thrown exceptions
    LMDBEither(try {
      keys.foldLeft(LMDBEither(cbf(keys))) {
          case (eBuilder, key) =>
            KeyBuffer.withBuf { keyBuf =>
              key.render(keyBuf) // ignore the red - this actually compiles
              val buf = safeExtract(db.get(tx, keyBuf))
              for {
                a <- sa.fromBuffer(buf)
                builder <- eBuilder
              } yield builder += a
            }
        }
      } finally {
        if (tx != null) tx.close()
      }).flatMap(_.map(_.result()))
  }



  /**
    * Gets and puts a new value according to a computation, does this transactionally
    * @param key - key at which to put
    * @param compute - computation to run
    */
  def transactionalGetAndSet[A](key: Key)(compute: A => LMDBEither[A])(implicit sa: Storable[A], instance: LMDBInstance): LMDBEither[A] = {

    // get a new transaction
    val tx = instance.env.txnWrite()

    // get the key
    // instantiate a result value, so there is something if it fails
    var res: LMDBEither[A] = NoResult.left
    try {
      res = for {
        bytes <- LMDBEither(
          KeyBuffer.withBuf {
            k =>
              key.render(k)
              db.get(tx, k)
          }
        )
        a <- sa.fromBuffer(safeExtract(bytes))
        res <- compute(a)
        _ <- LMDBEither(
          KeyBuffer.withBuf {
            k =>
              key.render(k)
              db.put(tx, k, sa.toBuffer(res))
          }
        )
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
  def transactionalAppendToSet[A](key: Key, a: A)(implicit sa: Storable[A], instance: LMDBInstance): LMDBEither[Unit] =
    transactionalGetAndSet[Set[A]](key){
      s => (s + a).right
    } map (_ => ())

  /**
    * Union a set of values to the existing one at a key
    */
  def transactionalUnion[A](key: Key, as: Set[A])(implicit sa: Storable[A], instance: LMDBInstance): LMDBEither[Unit] =
    transactionalGetAndSet[Set[A]](key) {
      oldAs => LMDBEither(oldAs union as)
    }.map(_ => ())

}
