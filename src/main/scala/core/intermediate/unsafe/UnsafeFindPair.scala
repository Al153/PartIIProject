package core.intermediate.unsafe

/**
  * Created by Al on 24/10/2017.
  *
  * Type-erased find pair datastructure
  * Need to limit construction to prevent end user-level programmers from constructing them
  */
sealed trait UnsafeFindPair {}
case class USAnd(l: UnsafeFindPair, r: UnsafeFindPair) extends UnsafeFindPair
case class USAndSingle(l: UnsafeFindPair, r: UnsafeFindSingle) extends UnsafeFindPair
case class USAtleast(n: Int, rel: UnsafeFindPair) extends UnsafeFindPair
case class USBetween(low: Int, high: Int, rel: UnsafeFindPair) extends UnsafeFindPair
case class USChain(l: UnsafeFindPair, r: UnsafeFindPair) extends UnsafeFindPair
case class USDistinct(r: UnsafeFindPair) extends UnsafeFindPair
case class USExactly(n: Int, rel: UnsafeFindPair) extends UnsafeFindPair
case object USId extends UnsafeFindPair
case class USNarrow(left: UnsafeFindPair, f: UnsafeFindable) extends UnsafeFindPair
case class USOr(left: UnsafeFindPair, right: UnsafeFindPair) extends UnsafeFindPair
case class USRel(r: ErasedRelationAttributes) extends UnsafeFindPair
case class USRevRel(r: ErasedRelationAttributes) extends UnsafeFindPair
case class USUpto(n: Int, rel: UnsafeFindPair) extends UnsafeFindPair

