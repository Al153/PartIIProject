package impl.lmdb

import java.nio.file.Files

import core.user.dsl.{DBDir, DatabaseAddress, E, Empty}
import core.user.interfaces._
import core.user.schema.SchemaDescription
import org.fusesource.lmdbjni.Env

import scala.concurrent.ExecutionContext
import scalaz.Scalaz._
import scalaz._


/**
  * Created by Al on 12/12/2017.
  * Bespoke LMDB backend for the system
  */

object LMDB extends DBBackend {
  override def open(address: DatabaseAddress, schema: SchemaDescription)(implicit e: ExecutionContext): \/[E, DBInstance] =
    try {
      val env = new Env()
      env.setMaxDbs(numberOfTables(schema))
      env.setMapSize(1024 * 1024 * 1024 * 1024 )
      val isNew = initEnvironment(env, address)
      println("number of tables  = " + numberOfTables(schema))


      val instance = new LMDBInstance(env, schema, isNew)
      for {
        _ <- instance.initialise()
      } yield instance
    } catch {case e: Throwable => errors.recoverLMDBException(e).left}

  /**
    * Counts number of tables needed
    * @param schema - schema to analyse
    * @return
    */
  private def numberOfTables(schema: SchemaDescription): Long = {
    val base = 8 // required no matter what: default, availableViews, Commits, object, view counter, relations, reverse relations
    val numberOfUserTables = schema.objects.size
    val numberOfIndices = schema.objects.foldLeft(0) {
      _ + _.any.pattern.length + 1 // index for each column + empty index
    }

    base + numberOfUserTables + numberOfIndices
  }

  private def initEnvironment(env: Env, address: DatabaseAddress): Boolean = {

    address match {
      case Empty =>
        val dir = Files.createTempDirectory("GraphDB")
        dir.toFile.deleteOnExit()
        env.open(dir.toFile.getPath)
        true

      case DBDir(dir, _, _) =>
        val directory = dir.toFile
        val isNew = if (!directory.exists()) {
          directory.mkdirs()
          true
        } else false
        env.open(directory.getPath)
        isNew
    }
  }
}


