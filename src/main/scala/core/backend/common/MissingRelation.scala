package core.backend.common

import core.error.E
import core.relations.RelationAttributes

case class MissingRelation[A, B](r: RelationAttributes[A, B]) extends E