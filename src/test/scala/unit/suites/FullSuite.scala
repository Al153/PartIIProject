package unit.suites

import unit.suites.individual._

/**
  * Created by Al on 27/12/2017.
  */
trait FullSuite extends HasBackend
  with Duplicates
  with IntersectionsAndDisjunctions
  with ReadWrite
  with Transitive
  with Repetition
  with LoopedRepetition
  with ComplexRepetition
  with SimplePathFinding
  with ComplexPathFinding
  with ViewSeparation
  with Indexing
  with FindSingleCases
