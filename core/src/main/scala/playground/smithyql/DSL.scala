package playground.smithyql

import cats.Id
import cats.implicits._
import cats.data.NonEmptyList

object DSL {

  implicit class StringDSLOps(val s: String) extends AnyVal {

    def call(
      args: (String, InputNode[Id])*
    ): Query[Id] = Query[Id](
      useClause = None,
      operationName = OperationName(s),
      input = struct(args: _*),
    )

  }

  implicit class QueryOps(val q: Query[Id]) extends AnyVal {

    def useService(path0: String, pathRest: String*)(service: String): Query[Id] = q.copy[Id](
      useClause = Some(
        UseClause[Id](QualifiedIdentifier(NonEmptyList(path0, pathRest.toList), service))
      )
    )

  }

  def struct(
    args: (String, InputNode[Id])*
  ): Struct[Id] = Struct[Id](Struct.Fields.fromSeq[Id](args.map(_.leftMap(Struct.Key(_)))))

  implicit def stringToAST(s: String): StringLiteral[Id] = StringLiteral[Id](s)
  implicit def intToAST(i: Int): IntLiteral[Id] = IntLiteral[Id](i)
  implicit def boolToAST(b: Boolean): BooleanLiteral[Id] = BooleanLiteral[Id](b)

  implicit def listToAST[A](
    l: List[A]
  )(
    implicit ev: A => InputNode[Id]
  ): Listed[Id] = Listed[Id](l.map(ev))

}
