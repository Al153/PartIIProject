package core.intermediate.unsafe

import core.backend.common.DBCell

/**
  * Created by Al on 25/10/2017.
  */
sealed trait UnsafeFindSingle {}
case class USFind(pattern: UnsafeFindable) extends UnsafeFindSingle
case class USFrom(start: UnsafeFindSingle, rel: UnsafeFindPair) extends UnsafeFindSingle
case class USNarrowS(start: UnsafeFindSingle, pattern: UnsafeFindable) extends UnsafeFindSingle