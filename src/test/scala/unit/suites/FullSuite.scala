package unit.suites

import core.user.dsl.E
import unit.suites.individual._

/**
  * Created by Al on 27/12/2017.
  */
trait FullSuite[E1 <: E] extends HasBackend[E1]
  with Duplicates[E1]
  with IntersectionsAndUnions[E1]
  with ReadWrite[E1]
  with Transitive[E1]
  with Repetition[E1]
  with LoopedRepetition[E1]
  with ComplexRepetition[E1]
  with SimplePathFinding[E1]
  with ComplexPathFinding[E1]
  with ViewSeparation[E1]
  with Indexing[E1]
  with FindSingleCases[E1]
