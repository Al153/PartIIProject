package unit.suites.individual

import core.backend.interfaces.DBBackend

import scala.concurrent.ExecutionContext


trait HasBackend {
  def backend: DBBackend
  implicit def ec: ExecutionContext
}