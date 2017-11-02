package unit.suites

import db.interfaces.DBBackend

import scala.concurrent.ExecutionContext


trait HasBackend {
  def backend: DBBackend
  implicit def ec: ExecutionContext
}