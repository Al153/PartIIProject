package core.intermediate

import core.RelationAttributes
import db.common.DBCell

/**
  * Created by Al on 24/10/2017.
  *
  * Type-erased find pair datastructure
  * Need to limit construction to prevent end user-level programmers from constructing them
  */
sealed trait UnsafeFindPair {}
case class USAnd(l: UnsafeFindPair, r: UnsafeFindPair) extends UnsafeFindPair
case class USAtleast(n: Int, rel: UnsafeFindPair) extends UnsafeFindPair
case class USBetween(low: Int, high: Int, rel: UnsafeFindPair) extends UnsafeFindPair
case class USChain(l: UnsafeFindPair, r: UnsafeFindPair) extends UnsafeFindPair
case class USExactly(n: Int, rel: UnsafeFindPair) extends UnsafeFindPair
case class USId() extends UnsafeFindPair
case class Narrow(left: UnsafeFindPair, pattern: Vector[Option[DBCell]]) extends UnsafeFindPair
case class USOr(left: UnsafeFindPair, right: UnsafeFindPair) extends UnsafeFindPair
case class USRel(r: RelationAttributes[Any, Any]) extends UnsafeFindPair
case class USRevRel(r: RelationAttributes[Any, Any]) extends UnsafeFindPair
case class USUpto(n: Int, rel: UnsafeFindPair) extends UnsafeFindPair

sealed trait UnsafeFindSingle {}
case class USFind(pattern: Vector[Option[DBCell]]) extends UnsafeFindSingle
case class USFrom(start: UnsafeFindSingle, rel: UnsafeFindPair) extends UnsafeFindSingle
case class USNarrowS(start: UnsafeFindSingle, pattern: Vector[Option[DBCell]]) extends UnsafeFindSingle