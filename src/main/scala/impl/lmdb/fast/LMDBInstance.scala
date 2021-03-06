package impl.lmdb.fast

import java.io.IOException
import java.nio.ByteBuffer
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}

import core.user.interfaces.DBInstance
import core.user.schema.SchemaDescription
import core.utils._
import impl.lmdb.common.LMDBEither
import impl.lmdb.common.interfaces.{LMDBExecutor, LMDBInstance}
import org.lmdbjava.Env

import scala.concurrent.ExecutionContext
import scalaz.Scalaz._

/**
  * Created by Al on 12/12/2017.
  *
  * [[DBInstance]] implementation
  */
sealed abstract class FastLMDBInstance(env: Env[ByteBuffer], schema: SchemaDescription)(implicit executionContext: ExecutionContext) extends LMDBInstance(env, schema) {

  /**
    * @return internal executor
    */
  override lazy val executor: LMDBExecutor = new FastLMDBExecutor()

}

/**
  * A temporary instance wants to delete its containing folder on closure, and do a full init
  */
final class TemporaryLMDBInstance(e: Env[ByteBuffer], schema: SchemaDescription, dir: Path)(implicit ec: ExecutionContext) extends FastLMDBInstance(e, schema) {
  override def initialise(): LMDBEither[Unit] = for {
    _ <- controlTables.availableViews.initialise()
    _ <- controlTables.commitsCounter.initialise()
    _ <- controlTables.defaultView.initialise()
    _ <- controlTables.relations.initialise()
    _ <- controlTables.reverseRelations.initialise()
    _ <- controlTables.viewsCounter.initialise()
    _ <- controlTables.viewsTable.initialise()
    _ <- controlTables.objectCounter.initialise()
    _ <- EitherOps.sequence(objects.map {case (_, table) => table.initialise()})
  } yield ()


  /**
    * Close DB and env
    */
  override def close(): Unit = {
    env.close()

    // recursively delete the database upon closure

    Files.walkFileTree(dir, new SimpleFileVisitor[Path] {
      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        Files.delete(file)
        FileVisitResult.CONTINUE
      }
      override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
        Files.delete(dir)
        FileVisitResult.CONTINUE
      }
    })
  }
}

 /**
   * A persistent LMDBInstance for a new database, needs initialisation, shouldn't delete on exit
   */
 final class NewLMDBInstance(
                              e: Env[ByteBuffer],
                              s: SchemaDescription
                            )(implicit ec: ExecutionContext) extends FastLMDBInstance(e, s) {
   override def initialise(): LMDBEither[Unit] = for {
     _ <- controlTables.availableViews.initialise()
     _ <- controlTables.commitsCounter.initialise()
     _ <- controlTables.defaultView.initialise()
     _ <- controlTables.relations.initialise()
     _ <- controlTables.reverseRelations.initialise()
     _ <- controlTables.viewsCounter.initialise()
     _ <- controlTables.viewsTable.initialise()
     _ <- controlTables.objectCounter.initialise()
     _ <- EitherOps.sequence(objects.map {case (_, table) => table.initialise()})
   } yield ()

   /**
     * Close the database
     */
   override def close(): Unit = env.close()
 }

/**
  * LMDBInstance for existing database, no initialisation or closure needed
  */
final class ExistingLMDBInstance(
                                  e: Env[ByteBuffer],
                                  s: SchemaDescription
                                )(implicit ec: ExecutionContext) extends FastLMDBInstance(e, s) {
  logger.trace("Existing!")
  override def initialise(): LMDBEither[Unit] = ().right
  /**
    * Close the database
    */
  override def close(): Unit = env.close()
}
