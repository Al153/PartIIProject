package remote

import core.user.containers.ConstrainedFuture
import core.user.dsl.{E, Empty}
import core.user.interfaces.{DBBackend, DBInstance}
import core.user.schema.SchemaDescription
import core.utils.EitherOps
import org.slf4j.Logger

import scala.concurrent.{Await, ExecutionContext}
import scalaz._, Scalaz._
import scala.concurrent.duration._

/**
  * Created by Al on 28/01/2018.
  *
  * Class to run remote tests and time them
  */
class RemoteTester(spec: TestSpec)(implicit logger: Logger, ec: ExecutionContext) {


  def runTest[A](
                  setup: DBInstance => ConstrainedFuture[E,Unit],
                  test: DBInstance => TestIndex =>  ConstrainedFuture[E, A],
                  schema: SchemaDescription
                ): Unit = {
    val runningTests = (for {
      ref <- spec.referenceImplementation.open(Empty, schema)
      instances <- EitherOps.sequence(spec.testImplementations.map{case (name, instance) => instance.open(Empty, schema).map(name -> _)})
    } yield (ref, instances)).fold(
      e => ConstrainedFuture.point(logError(e))(CaughtError.apply),
      {case (ref, i) => runBatches(setup, test, ref, i)}
    )
    Await.result(runningTests.run, (60*60*24).seconds) match {
      case \/-(()) => logger.info("[Done]")
      case -\/(e) => throw
    }
  }

  def doSetup(
               setup: (DBInstance) => ConstrainedFuture[E, Unit],
               instance: DBInstance,
               name: String
             ): ConstrainedFuture[E, Unit] = for {
    _ <- setup(instance)
    _ = logger.info(s"[Setup][Done]: Instance = $name")
  } yield ()

  def runBatches[A](setup: DBInstance => ConstrainedFuture[E, Unit], test: DBInstance => TestIndex =>  ConstrainedFuture[E, A], reference: DBInstance, toTest: Vector[(String, DBInstance)]): ConstrainedFuture[E, Unit] = {
    logger.info("Finished Setup")
    for {
      _ <- doSetup(setup, reference, "reference")
      refValues <- runReferenceBatch(reference, test, spec.batches)
      _ <- sequence(toTest) {
        case (name, instance) =>
          for {
            _ <- doSetup(setup, instance, name)
            _ <- runTestBatch(instance, name, test, refValues, spec.batches)

          } yield ()
      }
    } yield ()
  }

  def runBatch[A](
                   impl: DBInstance,
                   instanceName: String,
                   test: DBInstance => TestIndex =>  ConstrainedFuture[E, A],
                   n: TestIndex): ConstrainedFuture[E, Map[TestInstance, TimeResult[A]]] =
    sequence(TestInstance(instanceName)(n)) {
      timeConstrainedFuture(test(impl))
    }

  def runReferenceBatch[A](
                            impl: DBInstance,
                            test: DBInstance => TestIndex => ConstrainedFuture[E, A],
                            n: TestIndex
                          ): ConstrainedFuture[E, Map[TestIndex, A]] =
    for {
      instanceToTimedResult <- runBatch(impl, "ReferenceInstance", test, n)
      timeResults = instanceToTimedResult.map {case (t, res) => t.testIndex -> res}
      referenceResults = instanceToTimedResult.map {case (t, res) => t.testIndex -> res.a}
      _ = timeResults.values.map(logTimeResult(_, success = true))
      _ = logBatchResult(BatchedTimeResult(instanceToTimedResult.values.toSeq))
    } yield referenceResults


  def logTimeResult[A](ta: TimeResult[A], success: Boolean): A = {
    if (success){
      logger.info(s"[Result][Success][${ta.instance.testBackend}]:test = ${ta.instance.testIndex}: time = ${ta.ns}")
    } else {
      logger.info(s"[Result][Failure][${ta.instance.testBackend}]:test = ${ta.instance.testIndex}: time = ${ta.ns}")
    }
    ta.a
  }

  def logBatchResult[A](tas: BatchedTimeResult[A]): Map[TestInstance, A] = {
    logger.info(s"[Result][Batch]: backend = ${tas.testName}: testcount = ${tas.length}: totalTime = ${tas.fullTime}")
    tas.tas.map {r => r.instance -> r.a}.toMap
  }

  def runTestBatch[A](
                       impl: DBInstance,
                       toTest: String,
                       test: DBInstance => TestIndex => ConstrainedFuture[E, A],
                       expectedResults: Map[TestIndex, A],
                       n: TestIndex
                     ): ConstrainedFuture[E, Unit] = {
    for {
      instanceToTimedResult <- runBatch(impl, toTest, test, n)
      timeResults = instanceToTimedResult.map {case (t, res) => t.testIndex -> res}
      testResults = instanceToTimedResult.map {case (t, res) => t.testIndex -> res.a}
      isSuccess = testResults == expectedResults
      _ = timeResults.values.map(logTimeResult(_, success = isSuccess))
      _ = logBatchResult(BatchedTimeResult(instanceToTimedResult.values.toSeq))
    } yield ()
  }

  def logError(e: E): Unit = {
    logger.error("Hit an error when setting up instances. Halting tests")
    logger.error(e.toString)
  }

  def sequence[A, B](in: TraversableOnce[A])(f: A => ConstrainedFuture[E, B]): ConstrainedFuture[E, Map[A, B]] =
    in.foldLeft(ConstrainedFuture.point(Map.empty[A, B])(CaughtError(_): E)){
      case (or, a) =>
        for {
          r <- or
          b <- f(a)
        } yield r + (a -> b)
    }

  def timeConstrainedFuture[R](block: TestIndex => ConstrainedFuture[E, R])(testInstance: TestInstance): ConstrainedFuture[E, TimeResult[R]] = {
    val t0 = System.nanoTime()
    for {
      r <- block(testInstance.testIndex)
      t1 = System.nanoTime()
    } yield TimeResult(testInstance, t1 - t0,  r)
  }

}

case class CaughtError(e: Throwable) extends E

case class TestIndex(i: Int) extends AnyVal
object TestIndex {
  implicit class IntOps(i: Int) {
    def until(ti: TestIndex): IndexedSeq[TestIndex] =
      for (j <- i until ti.i) yield TestIndex(j)
  }
}

case class TestInstance(testBackend: String, testIndex: TestIndex)
object TestInstance {
  import TestIndex._
  def apply(testBackend: String)(topIndex: TestIndex): Seq[TestInstance] =
    for (i <- 1 until topIndex) yield TestInstance(testBackend, i)
}

case class TimeResult[A](instance: TestInstance, ns: Long, a: A)
case class BatchedTimeResult[A](tas: Seq[TimeResult[A]]) {
  def testName: String = tas.headOption.fold("NoTests"){ta => ta.instance.testBackend}
  def length: Int = tas.size
  def fullTime: Long = tas.foldLeft(0l){_ + _.ns}
}


trait TestSpec {
  def batches: TestIndex
  def referenceImplementation: DBBackend
  def testImplementations: Vector[(String, DBBackend)]
}
