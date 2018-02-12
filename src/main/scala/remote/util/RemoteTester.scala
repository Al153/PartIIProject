package remote.util

import core.user.containers.ConstrainedFuture
import core.user.dsl.{E, Empty}
import E._
import core.user.interfaces.{DBBackend, DBInstance}
import core.utils.EitherOps
import org.slf4j.Logger
import remote.{BatchedTimedResults, errorThrowable, ns}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}
import scalaz._

/**
  * Created by Al on 28/01/2018.
  *
  * Class to run remote tests and time them
  */
class RemoteTester(
                    referenceImplementation: DBBackend,
                    testImplementations: Vector[(String, DBBackend)]
                  )(implicit logger: Logger, ec: ExecutionContext) {


  def runTest[A](testSpec: TestSpec[A]): Unit = {
    val runningTests = (for {
      ref <- referenceImplementation.open(Empty, testSpec.schema)
      instances <- EitherOps.sequence(testImplementations.map{
        case (n, instance) =>
          instance.open(Empty, testSpec.schema).map(n -> _)
      })
    } yield (ref, instances)).fold(
      {e =>
        logger.info("Error thrown " + e)
        ConstrainedFuture.point(logError(e))},
      {case (ref, i) => runBatches(testSpec, ref, i)}
    )
    Await.result(runningTests.run, (60*60*24*365).seconds) match {
      case \/-(()) => logger.info("[Done]")
      case -\/(e) => errorThrowable(e)
    }
  }

  private def doSetup(
               setup: (DBInstance) => ConstrainedFuture[E, Unit],
               instance: DBInstance,
               name: String
             ): ConstrainedFuture[E, Unit] = for {
    _ <- setup(instance)
    _ = logger.info(s"[Setup][Done]: Instance = $name")
  } yield logger.info(s"[Setup][Done]: Instance = $name")

  private def runBatches[A](spec: TestSpec[A], reference: DBInstance, toTest: Vector[(String, DBInstance)]): ConstrainedFuture[E, Unit] = {
    logger.info("Starting Setup")
    for {
      _ <- doSetup(spec.setup, reference, "reference")
      _ = logger.info("[Starting reference batch]")
      refValues <- runReferenceBatch(spec, reference)
      _ = logger.info("[Done reference batch]")
      _ <- sequence(toTest) {
        case (name, instance) =>
          logger.info(s"[Starting test for $name] ")
          (for {
            _ <- doSetup(spec.setup, instance, name)
            _ = logger.info(s"[Done setup for $name] ")
            _ <- runTestBatch(spec, instance, name, refValues)
            _ = logger.info(s"[Run test for $name] ")
          } yield ()).recover(_ => ())
      }
    } yield ()
  }

  private def runBatch[A](
                   spec: TestSpec[A],
                   impl: DBInstance,
                   instanceName: String
                 ): ConstrainedFuture[E, Map[TestInstance, TimeResult[Int]]] =
    sequence(TestInstance(spec, instanceName)) {
      i =>
        logger.info(s"Running $i" )
        timeConstrainedFuture(spec.test(impl))(i)
    }

  private def runReferenceBatch[A](
                            spec: TestSpec[A],
                            impl: DBInstance
                          ): ConstrainedFuture[E, Map[TestIndex, Int]] =
    for {
      instanceToTimedResult <- runBatch(spec, impl, "ReferenceInstance")
      timeResults = instanceToTimedResult.map {case (t, res) => t.testIndex -> res}
      referenceResults = instanceToTimedResult.map {case (t, res) => t.testIndex -> res.a}
      _ = timeResults.values.foreach(logTimeResult(_, success = true))
      _ = logBatchResult(BatchedTimedResults(instanceToTimedResult.values.toSeq))
    } yield referenceResults.mapValues(_.hashCode())


  def logTimeResult[A](ta: TimeResult[A], success: Boolean): Unit = {
    if (success){
      logger.info(s"[Result][Success][${ta.instance.testBackend}]:${ta.instance}:time=${ns(ta.ns)}")
    } else {
      logger.info(s"[Result][Failure][${ta.instance.testBackend}]:${ta.instance}:time=${ns(ta.ns)}")
    }
  }

  private def logBatchResult[A](tas: BatchedTimedResults[A]): Unit = {
    logger.info(s"[Result][Batch]:test=${tas.testName.name}:impl=${tas.backend}:testcount=${tas.length}:totalTime=${ns(tas.fullTime)}")
  }

  private def runTestBatch[A](
                       spec: TestSpec[A],
                       impl: DBInstance,
                       toTest: String,
                       expectedResults: Map[TestIndex, Int]
                     ): ConstrainedFuture[E, Unit] = {
    for {
      instanceToTimedResult <- runBatch(spec, impl, toTest)
      timeResults = instanceToTimedResult.map {case (t, res) => t.testIndex -> res}
      testResults = instanceToTimedResult.map {case (t, res) => t.testIndex -> res.a}
      isSuccess = testResults == expectedResults
      _ = timeResults.values.foreach(logTimeResult(_, success = isSuccess))
      _ = logBatchResult(BatchedTimedResults(instanceToTimedResult.values.toSeq))
    } yield ()
  }

  private def logError(e: E): Unit = {
    logger.error("Hit an error when setting up instances. Halting tests")
    logger.error(e.toString)
  }

  private def sequence[A, B](in: TraversableOnce[A])(f: A => ConstrainedFuture[E, B]): ConstrainedFuture[E, Map[A, B]] =
    in.foldLeft(ConstrainedFuture.point(Map.empty[A, B])){
      case (or, a) =>
        for {
          r <- or
          b <- f(a)
        } yield r + (a -> b)
    }

  private def timeConstrainedFuture[R](block: TestIndex => ConstrainedFuture[E, R])(testInstance: TestInstance): ConstrainedFuture[E, TimeResult[Int]] = {
    val t0 = System.nanoTime()
    for {
      r <- block(testInstance.testIndex)
      t1 = System.nanoTime()
    } yield TimeResult(testInstance, t1 - t0,  r.hashCode())
  }
}










