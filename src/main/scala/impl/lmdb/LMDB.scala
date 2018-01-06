package impl.lmdb

import java.nio.file.Files

import core.user.interfaces._
import core.user.containers.ConstrainedFuture
import core.user.dsl.{DBDir, DatabaseAddress, E, Empty}
import core.user.schema.SchemaDescription
import org.fusesource.lmdbjni.Env

import scala.concurrent.ExecutionContext


/**
  * Created by Al on 12/12/2017.
  * Bespoke LMDB backend for the system
  */

// todo: design key value system for system


object LMDB extends DBBackend {
  override def open(address: DatabaseAddress, schema: SchemaDescription)(implicit e: ExecutionContext): ConstrainedFuture[E, DBInstance] =
    ConstrainedFuture.point[E, DBInstance] {
      val env = initEnvironment(address)
      new LMDBInstance(env, schema)
    }(errors.recoverLMDBException)

  private def initEnvironment(address: DatabaseAddress): Env = {

    address match {
      case Empty =>
        val dir = Files.createTempDirectory("GraphDB")
        dir.toFile.deleteOnExit()
        new Env(dir.toFile.getPath)

      case DBDir(dir, _, _) =>
        val directory = dir.toFile
        if (!directory.exists()) directory.mkdirs()

        new Env(directory.getPath)
    }
  }
}


