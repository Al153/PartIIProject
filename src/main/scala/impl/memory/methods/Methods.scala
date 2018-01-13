package impl.memory.methods

/**
  * Methods object composed of several methods
  */
trait Methods
  extends ExecutorMethods
    with SetImpl
    with VectorImpl
    with Joins
    with Writes
    with PathFinding
    with RepetitionImpl