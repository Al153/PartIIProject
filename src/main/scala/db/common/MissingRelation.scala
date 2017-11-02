package db.common

import core.RelationAttributes
import core.error.E

case class MissingRelation[A, B](r: RelationAttributes[A, B]) extends E