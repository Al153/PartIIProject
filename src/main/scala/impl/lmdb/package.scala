package impl

import core.containers.ConstrainedFuture
import core.error.E
import core.view.View
import impl.lmdb.access.{Key, Storeable}
import impl.lmdb.errors.{LMDBError, _}

import scala.concurrent.{ExecutionContext, Promise}
import scalaz.Scalaz._
import scalaz._

/**
  * Created by Al on 28/12/2017.
  */
package object lmdb {

  type LMDBEither[A] = LMDBError \/ A
  type LMDBFuture[A] = ConstrainedFuture[LMDBError, A]

  def LMDBFutureE[A](ea: LMDBEither[A])(implicit ec: ExecutionContext): LMDBFuture[A] = ConstrainedFuture.either(ea)(recoverLMDBException)
  def LMDBFuture[A](a: => A)(implicit ec: ExecutionContext): LMDBFuture[A] =  ConstrainedFuture.point(a)(recoverLMDBException)
  def LMDBFutureI[A](a: => A)(implicit ec: ExecutionContext): LMDBFuture[A] = ConstrainedFuture.future(Promise.successful(a.right).future)(recoverLMDBException)
  def LMDBFutureER[A](ea: LMDBEither[A])(implicit ec: ExecutionContext): LMDBFuture[A] = ConstrainedFuture.future(Promise.successful(ea).future)(recoverLMDBException)
  def LMDBFailure[A](e: => LMDBError)(implicit ec: ExecutionContext): LMDBFuture[A] = ConstrainedFuture.future(Promise.successful(e.left).future)(recoverLMDBException)
  def LMDBEither[A](a: => A): LMDBEither[A] = try { a.right} catch {case e: Throwable => recoverLMDBException(e).left}
  def safeEither[A](ea: => LMDBEither[A]): LMDBEither[A] = try {ea} catch {case e: Throwable => recoverLMDBException(e).left}
  def LMDBLeft[A](e: => LMDBError): LMDBEither[A] = try {e.left} catch {case e: Throwable => recoverLMDBException(e).left}

  def asE(s: LMDBError): E = s
  def asEither[A](sQLEither: LMDBEither[A]): E \/ A = sQLEither.leftMap(asE)
  def asCFuture[A](f: LMDBFuture[A]): ConstrainedFuture[E, A] = f.leftMap(asE)

  implicit class LMDBFutureOps[A](u: LMDBFuture[A]) {
    def asCFuture: ConstrainedFuture[E, A] = lmdb.asCFuture(u)
  }

  implicit class LMDBEitherOps[A](u: LMDBEither[A]) {
    def asEither: E \/ A = lmdb.asEither(u)
  }

  def put[A](key: Key, a: A)(implicit sa: Storeable[A], instance: LMDBInstance): LMDBEither[Unit] = LMDBEither {
    instance.db.put(key.render, sa.toBytes(a).toArray)
  }

  def get[A](key: Key)(implicit sa: Storeable[A], instance: LMDBInstance): LMDBEither[A] = for {
    b <- LMDBEither(instance.db.get(key.render))
    r <- sa.fromBytes(b.toVector)
  } yield r

  def transactionalGetAndSet[A](key: Key)(compute: A => LMDBEither[A])(implicit sa: Storeable[A], instance: LMDBInstance): LMDBEither[A] = {
    import org.fusesource.lmdbjni.Transaction

    val tx: Transaction = instance.env.createWriteTransaction()
    val k = key.render
    var res: LMDBEither[A] = NoResult.left
    try {
        res = for {
          bytes <- LMDBEither(instance.db.get(tx, k))
          a <- sa.fromBytes(bytes.toVector)
          res <- compute(a)
          _ <- LMDBEither(instance.db.put(tx, k, sa.toBytes(res).toArray))
        } yield a
        res

    } finally {
      // Make sure you either commit or rollback to avoid resource leaks.
      if (res.isRight) tx.commit()
      else tx.abort()
    }
  }

  def transactionalAppendToSet[A](key: Key, a: A)(implicit sa: Storeable[A], instance: LMDBInstance): LMDBEither[Unit] =
    transactionalGetAndSet[Set[A]](key){
      s => (s + a).right
    } map (_ => ())

  def transactionalUnion[A](key: Key, as: Set[A])(implicit sa: Storeable[A], instance: LMDBInstance): LMDBEither[Unit] =
    transactionalGetAndSet[Set[A]](key) {
      oldAs => LMDBEither(oldAs union as)
    }.map(_ => ())

  val initialView = View(0)

}
