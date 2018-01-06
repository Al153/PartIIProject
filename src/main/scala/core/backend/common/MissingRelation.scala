package core.backend.common

import core.user.dsl.{E, RelationAttributes}

case class MissingRelation(r: RelationAttributes[_, _])