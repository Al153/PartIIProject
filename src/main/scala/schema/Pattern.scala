package schema

import core.intermediate
import core.intermediate.GetNode
import schema.typeclasses.PrimitiveDatabaseValue

/**
  * Created by Al on 01/10/2017
  * we want to do patternmatching on schema case classes
  */
object Pattern {
  sealed trait Pattern[Actual] {
  }

  case class Fixed[A](a: A) extends Pattern[A]{}

  implicit def fix[A](a: A): Pattern[A] = Fixed(a)

  case class Pattern0 [Actual]() extends Pattern[Actual] {
    def apply(): Pattern0 [Actual] = new Pattern0[Actual]()
  }
  case class Pattern1 [Actual, T1](a1: Pattern[T1]) extends Pattern[Actual] {
    def apply[A1](
                   a1: A1
                 )( implicit
                    c1: A1 => Pattern[T1]
    ): Pattern1[Actual, T1] = Pattern1(a1)
  }
  case class Pattern2 [Actual, T1, T2](a1: Pattern[T1], a2: Pattern[T2]) extends Pattern[Actual] {
    def apply[A1, A2](
                       a1: A1,
                       a2: A2
                     )( implicit
                        c1: A1 => Pattern[T1],
                        c2: A2 => Pattern[T2]
    ): Pattern2[Actual, T1, T2] = Pattern2(a1, a2)
  }
  case class Pattern3 [Actual, T1, T2, T3](a1: Pattern[T1], a2: Pattern[T2], a3: Pattern[T3]) extends Pattern[Actual] {
    def apply[A1, A2, A3](
                           a1: A1,
                           a2: A2,
                           a3: A3
                         )( implicit
                            c1: A1 => Pattern[T1],
                            c2: A2 => Pattern[T2],
                            c3: A3 => Pattern[T3]
    ): Pattern3[Actual, T1, T2, T3] = Pattern3(a1, a2, a3)
  }
  case class Pattern4 [Actual, T1, T2, T3, T4](a1: Pattern[T1], a2: Pattern[T2], a3: Pattern[T3], a4: Pattern[T4]) extends Pattern[Actual] {
    def apply[A1, A2, A3, A4](
                               a1: A1,
                               a2: A2,
                               a3: A3,
                               a4: A4
                             )( implicit
                                c1: A1 => Pattern[T1],
                                c2: A2 => Pattern[T2],
                                c3: A3 => Pattern[T3],
                                c4: A4 => Pattern[T4]
    ): Pattern4[Actual, T1, T2, T3, T4] = Pattern4(a1, a2, a3, a4)
  }
  case class Pattern5 [Actual, T1, T2, T3, T4, T5](a1: Pattern[T1], a2: Pattern[T2], a3: Pattern[T3], a4: Pattern[T4], a5: Pattern[T5]) extends Pattern[Actual] {
    def apply[A1, A2, A3, A4, A5](
                                   a1: A1,
                                   a2: A2,
                                   a3: A3,
                                   a4: A4,
                                   a5: A5
                                 )( implicit
                                    c1: A1 => Pattern[T1],
                                    c2: A2 => Pattern[T2],
                                    c3: A3 => Pattern[T3],
                                    c4: A4 => Pattern[T4],
                                    c5: A5 => Pattern[T5]
    ): Pattern5[Actual, T1, T2, T3, T4, T5] = Pattern5(a1, a2, a3, a4, a5)
  }
  case class Pattern6 [Actual, T1, T2, T3, T4, T5, T6](a1: Pattern[T1], a2: Pattern[T2], a3: Pattern[T3], a4: Pattern[T4], a5: Pattern[T5], a6: Pattern[T6]) extends Pattern[Actual] {
    def apply[A1, A2, A3, A4, A5, A6](
                                       a1: A1,
                                       a2: A2,
                                       a3: A3,
                                       a4: A4,
                                       a5: A5,
                                       a6: A6
                                     )( implicit
                                        c1: A1 => Pattern[T1],
                                        c2: A2 => Pattern[T2],
                                        c3: A3 => Pattern[T3],
                                        c4: A4 => Pattern[T4],
                                        c5: A5 => Pattern[T5],
                                        c6: A6 => Pattern[T6]
    ): Pattern6[Actual, T1, T2, T3, T4, T5, T6] = Pattern6(a1, a2, a3, a4, a5, a6)
  }
  case class Pattern7 [Actual, T1, T2, T3, T4, T5, T6, T7](a1: Pattern[T1], a2: Pattern[T2], a3: Pattern[T3], a4: Pattern[T4], a5: Pattern[T5], a6: Pattern[T6], a7: Pattern[T7]) extends Pattern[Actual] {
    def apply[A1, A2, A3, A4, A5, A6, A7](
                                                        a1: A1,
                                                        a2: A2,
                                                        a3: A3,
                                                        a4: A4,
                                                        a5: A5,
                                                        a6: A6,
                                                        a7: A7
                                                      )( implicit
                                                         c1: A1 => Pattern[T1],
                                                         c2: A2 => Pattern[T2],
                                                         c3: A3 => Pattern[T3],
                                                         c4: A4 => Pattern[T4],
                                                         c5: A5 => Pattern[T5],
                                                         c6: A6 => Pattern[T6],
                                                         c7: A7 => Pattern[T7]
                                                      ): Pattern7[Actual, T1, T2, T3, T4, T5, T6, T7] = Pattern7(1, a2, a3, a4, a5, a6, a7)
  }
  case class Pattern8 [Actual, T1, T2, T3, T4, T5, T6, T7, T8](a1: Pattern[T1], a2: Pattern[T2], a3: Pattern[T3], a4: Pattern[T4], a5: Pattern[T5], a6: Pattern[T6], a7: Pattern[T7], a8: Pattern[T8]) extends Pattern[Actual] {
    def apply[A1, A2, A3, A4, A5, A6, A7, A8](
                                                        a1: A1,
                                                        a2: A2,
                                                        a3: A3,
                                                        a4: A4,
                                                        a5: A5,
                                                        a6: A6,
                                                        a7: A7,
                                                        a8: A8
                                             )( implicit
                                                c1: A1 => Pattern[T1],
                                                c2: A2 => Pattern[T2],
                                                c3: A3 => Pattern[T3],
                                                c4: A4 => Pattern[T4],
                                                c5: A5 => Pattern[T5],
                                                c6: A6 => Pattern[T6],
                                                c7: A7 => Pattern[T7],
                                                c8: A8 => Pattern[T8]
    ): Pattern8[Actual, T1, T2, T3, T4, T5, T6, T7, T8] = Pattern8(a1, a2, a3, a4, a5, a6, a7, a8)
  }
  case class Pattern9 [Actual, T1, T2, T3, T4, T5, T6, T7, T8, T9](a1: Pattern[T1], a2: Pattern[T2], a3: Pattern[T3], a4: Pattern[T4], a5: Pattern[T5], a6: Pattern[T6], a7: Pattern[T7], a8: Pattern[T8], a9: Pattern[T9]) extends Pattern[Actual] {
    def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9](
                                                   a1: A1,
                                                   a2: A2,
                                                   a3: A3,
                                                   a4: A4,
                                                   a5: A5,
                                                   a6: A6,
                                                   a7: A7,
                                                   a8: A8,
                                                   a9: A9
                                                 )( implicit
                                                    c1: A1 => Pattern[T1],
                                                    c2: A2 => Pattern[T2],
                                                    c3: A3 => Pattern[T3],
                                                    c4: A4 => Pattern[T4],
                                                    c5: A5 => Pattern[T5],
                                                    c6: A6 => Pattern[T6],
                                                    c7: A7 => Pattern[T7],
                                                    c8: A8 => Pattern[T8],
                                                    c9: A9 => Pattern[T9]
    ): Pattern9[Actual, T1, T2, T3, T4, T5, T6, T7, T8, T9] = Pattern9(a1, a2, a3, a4, a5, a6, a7, a8, a9)
  }

  case class Pattern10[Actual, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](a1: Pattern[T1], a2: Pattern[T2], a3: Pattern[T3], a4: Pattern[T4], a5: Pattern[T5], a6: Pattern[T6], a7: Pattern[T7], a8: Pattern[T8], a9: Pattern[T9], a10: Pattern[T10]) extends Pattern[Actual] {
    def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
               a1: A1,
               a2: A2,
               a3: A3,
               a4: A4,
               a5: A5,
               a6: A6,
               a7: A7,
               a8: A8,
               a9: A9,
               a10: A10
             )( implicit
                c1: A1 => Pattern[T1],
                c2: A2 => Pattern[T2],
                c3: A3 => Pattern[T3],
                c4: A4 => Pattern[T4],
                c5: A5 => Pattern[T5],
                c6: A6 => Pattern[T6],
                c7: A7 => Pattern[T7],
                c8: A8 => Pattern[T8],
                c9: A9 => Pattern[T9],
                c10: A10 => Pattern[T10]
    ): Pattern10[Actual, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] = Pattern10(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
  }


  def ?[T](implicit protoType: Pattern[T]) = protoType

  // Primitives can be a single unknown
  implicit def prototypePrimitive[T](implicit p: PrimitiveDatabaseValue[T]): Pattern[T] = Pattern0[T]()
}
