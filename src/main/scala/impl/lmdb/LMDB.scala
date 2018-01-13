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
      val (env, isNew) = initEnvironment(address)
      val instance = new LMDBInstance(env, schema, isNew)
      for {
        _ <- instance.initialise()
      } yield instance
    } catch {case e: Throwable => errors.recoverLMDBException(e).left}

  private def initEnvironment(address: DatabaseAddress): (Env, Boolean) = {

    address match {
      case Empty =>
        val dir = Files.createTempDirectory("GraphDB")
        dir.toFile.deleteOnExit()
        (new Env(dir.toFile.getPath), true)

      case DBDir(dir, _, _) =>
        val directory = dir.toFile
        val isNew = if (!directory.exists()) {
          directory.mkdirs()
          true
        } else false

        (new Env(directory.getPath), isNew)
    }
  }
}


