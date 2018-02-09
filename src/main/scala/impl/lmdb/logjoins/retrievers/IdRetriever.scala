package impl.lmdb.logjoins.retrievers

import core.utils._

import scalaz.Scalaz._
import scalaz._

/**
  * Created by Al on 06/02/2018.
  */

object IdRetriever extends UncachedRelationRetriever(
  objIds => objIds.mapPair.right,
  objId => Set(objId).right
)
