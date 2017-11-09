package impl.sql.adt

import core.containers.Operation
import core.error.E
import core.intermediate.unsafe.{USId, _}
import core.view.View
import impl.sql.adt._
import impl.sql.compile.{BindOrGetRel, Context}
import impl.sql.tables.ObjectTable

sealed trait SQLOperation {
  def commitDefinition: GetView // view
  def definitions: List[SQLDefinition]
  def body: SQLPair
  def execute: Operation[E, SQLResult]

  def startTable: ObjectTable
  def endTable: ObjectTable

  def getQuery: String = {
    def defString(defs: List[SQLDefinition]) = {
      val commitDef = s"with commit as (${commitDefinition.getSQL}),\n"
      commitDef + definitions.map(defn => s"${defn.varName} as ${SQLDefinition.getQuery(defn)}" )
    }
  }
}



object SQLOperation {
  def fromIntermediate(view: View, pair: UnsafeFindPair): SQLOperation = {
    def auxSingle(q: UnsafeFindSingle): Context[SQLSingle] = ???

    def aux(f: UnsafeFindPair): Context[SQLPair] =
      pair match {
        case USAnd(l, r) =>
          for {
            sl <- aux(l)
            sr <- aux(r)
          } yield Intersection(sl, sr)

        case USAndSingle(l, r) =>
          for {
            sl <- aux(l)
            sr <- auxSingle(r)
          } yield Narrow(sl, sr)

        case USChain(l, r) =>
          for {
            sl <- aux(l)
            sr <- aux(r)
          } yield Chain(sl, sr)

        case USDistinct(r) =>
          for {
            sr <- aux(r)
          } yield Distinct(sr)

        case USId(table) =>
          Context.point(Id(table))

        case USNarrow(rel, filter) =>
          for {
            sl <- aux(rel)
            sr <- auxSingle(USFind(filter))
          } yield Narrow(sl, sr)

        case USOr(l, r) =>
          for {
            sl <- aux(l)
            sr <- aux(r)
          } yield Intersection(sl, sr)


        case USRel(r) =>
          for {
            name <- BindOrGetRel(r)
          } yield JoinRelation(name)

        case USRevRel(r) =>
          for {
            name <- BindOrGetRel(r)
          } yield  JoinReverseRelation(name)
      }

  }

}