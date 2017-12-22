package core.backend.common

import core.error.E
import core.relations.RelationAttributes

case class MissingRelation(r: RelationAttributes[_, _]) extends E