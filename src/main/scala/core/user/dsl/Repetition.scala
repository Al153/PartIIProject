package core.user.dsl

/**
  * A sealed trait hierarchy and syntax for initiating repetition
  */

sealed trait Repetition
case class UptoRange private (n: Int) extends Repetition
case class BetweenRange private(lo: Int, hi: Int) extends Repetition
case class AtleastRange private (n: Int) extends Repetition

trait RepetitionOps {

  /**
    * Syntax for creating [[Repetition]]s from ints
    */
  implicit class RepetitionSyntax(u: Int) {
    /**
      * Create a [[BetweenRange]]
      */
    def -->(that: Int): Repetition = until(that)
    /**
      * Create an [[AtleastRange]]
      */
    def ++ : Repetition = upwards

    /**
      * Create an [[UptoRange]]
      */
    def ->: : Repetition = upto
    /**
      * Create an [[AtleastRange]]
      */
    def :-> : Repetition = upwards


    /**
      * Create a [[BetweenRange]]
      */
    def until(that: Int): Repetition = if (u <= that) BetweenRange(u, that) else BetweenRange(that, u)
    /**
      * Create an [[AtleastRange]]
      */
    def upwards: Repetition = AtleastRange(u)
    /**
      * Create an [[UptoRange]]
      */
    def upto: Repetition = UptoRange(u)
  }
  /**
    * Create an [[UptoRange]]
    */
  def upto(n: Int): Repetition = UptoRange(n)


}