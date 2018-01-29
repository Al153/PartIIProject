package remote

import core.user.containers.Operation
import core.user.dsl.{E, Empty}
import core.user.interfaces.{DBBackend, DBInstance}
import core.user.schema.SchemaDescription
import core.utils.EitherOps

import scala.collection.generic.CanBuildFrom

/**
  * Created by Al on 28/01/2018.
  *
  * Class to run remote tests and time them
  */
class RemoteTester(spec: TestSpec) {


  def runTest[A](
                  setup: DBInstance => Operation[E,Unit],
                  test: DBInstance => Int =>  Operation[E, A],
                  schema: SchemaDescription
                ): Unit = {
    (for {
      ref <- spec.referenceImplementation.open(Empty, schema)
      instances <- EitherOps.sequence(spec.testImplementations.map(_.open(Empty, schema)))
    } yield (ref, instances)).fold(
      logError,
      {case (ref, i) => runBatches(test, ref, i)}
    )
  }

  def runBatches[A](test: DBInstance => Int =>  Operation[E, A], reference: DBInstance, toTest: Vector[DBInstance]): Unit = {
    logger.info("Finished Setup")

    for (batchIndex <- 1 until spec.batches) {
      for {
        refValue <- runBatch(reference, test, spec.perBatch)
        // todo: properly sequence
      } yield ???

    }
  }

  def runBatch[A](impl: DBInstance, test: DBInstance => Int =>  Operation[E, A], n: Int): Operation[E, Vector[A]] =
    sequence(Vector.fill(n)(())) {
      _ => test(impl)(n)
    }

  def logError(e: E): Unit = {
    logger.error("Hit an error when setting up instances. Halting tests")
    logger.error(e.toString)
  }

  def sequence[E, A, B, M[X] <: TraversableOnce[X]](
                                                     in: M[A]
                                                   )(
    f: A => Operation[E, B]
  )(implicit cbf: CanBuildFrom[M[A], B, M[B]]): Operation[E, M[B]] =
    in.foldLeft(Operation.point(cbf(in), CaughtError)){
      case (or, a) =>
        for {
          r <- or
          b <- f(a)
        } yield r += b
    }.map(_.result())

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }

}

case class CaughtError(e: Throwable) extends E

case class TestInstance(testBackend: Int, testIndex: Int)

trait TestSpec {
  def batches: Int
  def referenceImplementation: DBBackend
  def testImplementations: Vector[DBBackend]
}
