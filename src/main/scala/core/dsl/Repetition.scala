package core.dsl

sealed trait Repetition
case class UptoRange private (n: Int) extends Repetition
case class BetweenRange private(lo: Int, hi: Int) extends Repetition
case class AtleastRange private (n: Int) extends Repetition

object Repetition {

  implicit class RepetitionSyntax(u: Int) {
    def -->(that: Int): Repetition = until(that)
    def ++ : Repetition = upwards
    def ->: : Repetition = upto
    def :-> : Repetition = upwards



    def until(that: Int): Repetition = if (u <= that) BetweenRange(u, that) else BetweenRange(that, u)
    def upwards: Repetition = AtleastRange(u)
    def upto: Repetition = UptoRange(u)
  }

  def upto(n: Int): Repetition = UptoRange(n)


}