package impl.lmdb

import impl.lmdb.common.access.ObjId
import org.slf4j.{Logger, LoggerFactory}
import core.utils._

package object logjoins {
  val logger: Logger = LoggerFactory.getLogger(this.getClass)


  implicit class ResultSetOps(m: Map[ObjId, Set[ObjId]]){
    def prune: Map[ObjId, Set[ObjId]] = m.filter{case (_, s) => s.nonEmpty}

    def intersect(n: Map[ObjId, Set[ObjId]]): Map[ObjId, Set[ObjId]] = {
      val keys = m.keySet intersect  n.keySet
      val res = Map.newBuilder[ObjId, Set[ObjId]]
      for (key <- keys) {
        if ((key in m) && (key in n)) {
          res += (key -> (m(key) intersect n(key)))
        }
      }

      res.result()
    }

    def union(n: Map[ObjId, Set[ObjId]]): Map[ObjId, Set[ObjId]] = {
      val keys = m.keySet union n.keySet
      val res = Map.newBuilder[ObjId, Set[ObjId]]
      for (key <- keys) {
        if ((key in m) || (key in n)) {
          res += (key -> (m.getOrElse(key, Set()) union n.getOrElse(key, Set())))
        }
      }
      res.result()
    }

    def renderPairs: Set[(ObjId, ObjId)] = {
      val res = Set.newBuilder[(ObjId, ObjId)]
      for {
        (left, set) <- m
        right <- set
      } res += (left -> right)

      res.result()
    }
  }
}