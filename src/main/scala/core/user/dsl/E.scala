package core.user.dsl

/**
  * Created by Al on 03/10/2017.
  *
  * Error trait
  */
trait E

object E {
  implicit object ERecovery extends HasRecovery[E] {
    override def recover(t: Throwable): E = new E {
      override def toString: String = "Caught general exception: " + t.toString + "Cause = " + t.getCause
    }
  }
}


