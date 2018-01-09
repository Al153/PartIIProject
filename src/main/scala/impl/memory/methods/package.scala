package impl.memory

/**
  * Methods object composed of several methods
  */
package object methods
  extends ExecutorMethods
    with SetImpl
    with VectorImpl
    with Joins
    with Writes
    with PathFinding
    with RepetitionImpl