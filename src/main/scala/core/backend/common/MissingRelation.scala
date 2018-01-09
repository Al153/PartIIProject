package core.backend.common

import core.user.dsl.RelationAttributes

/**
  * Small class to allow backends to complain about missing relations
  * @param r
  */

case class MissingRelation(r: RelationAttributes[_, _])