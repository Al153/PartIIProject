package core.user.dsl

/**
  * Typeclass for allowing a type to be recovered
  * @tparam E
  */
trait HasRecovery[E] {
  def recover(t: Throwable): E
}