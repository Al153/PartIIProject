package impl.memory.methods

import impl.memory.{MemoryObject, RelatedPair}

trait Joins {
  // slow join

  protected def join(leftRes: Vector[RelatedPair], rightRes: Vector[RelatedPair]): Vector[RelatedPair] = {
    println("\n\n\n\nJoin, left = " + leftRes.mkString("\n\t\t"))
    println("join, right = " + rightRes.mkString("\n\t\t"))

    // build an index of all values to join, prevents overduplication
    val collectedLeft = leftRes.foldLeft(Map[MemoryObject, Vector[MemoryObject]]()) {
      case (m, pair) =>
        m + (pair._2 -> (m.getOrElse(pair._2, Vector[MemoryObject]()) :+ pair._1))
    }

    println("Join, index = " + collectedLeft.mkString("\n\t\t"))

    val res = for {
      (middle, to) <- rightRes
      from <- collectedLeft.getOrElse(middle, Vector())
    } yield (from, to)

    println("Join, res = " + res.mkString("\n\t\t"))
    res
  }


  protected def joinSet(leftRes: Set[RelatedPair], rightRes: Set[RelatedPair]): Set[RelatedPair] = {
    println("\n\n\n\nJoin, left = " + leftRes.mkString("\n\t\t"))
    println("join, right = " + rightRes.mkString("\n\t\t"))

    // build an index of all values to join, prevents overduplication
    val collectedLeft = leftRes.foldLeft(Map[MemoryObject, Set[MemoryObject]]()) {
      case (m, pair) =>
        m + (pair._2 -> (m.getOrElse(pair._2, Set[MemoryObject]()) ++ Set(pair._1)))
    }

    println("Join, index = " + collectedLeft.mkString("\n\t\t"))

    val res = for {
      (middle, to) <- rightRes
      from <- collectedLeft.getOrElse(middle, Set())
    } yield (from, to)

    println("Join, res = " + res.mkString("\n\t\t"))
    res
  }
}