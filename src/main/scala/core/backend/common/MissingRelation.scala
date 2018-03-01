package core.backend.common

import core.user.dsl.Relation

/**
  * Small class to allow backends to complain about missing relations
  * @param r
  */

case class MissingRelation(r: Relation[_, _])