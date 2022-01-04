package playground

import cats.Id

object AST {

  type AST = AST.high.AST[Id]
  type Query = AST.high.Query[Id]
  val Query = AST.high.Query
  type Struct = AST.high.Struct[Id]
  val Struct = AST.high.Struct
  type IntLiteral = AST.high.IntLiteral[Id]
  val IntLiteral = AST.high.IntLiteral
  type StringLiteral = AST.high.StringLiteral[Id]
  val StringLiteral = AST.high.StringLiteral

  object high {

    case class Query[F[_]](
      operationName: F[String],
      input: F[Struct[F]],
    )

// todo bad name, Query isn't an AST but is part of the AST
    sealed trait AST[F[_]] extends Product with Serializable

    case class Struct[F[_]](
      fields: F[Map[F[String], F[AST[F]]]]
    ) extends AST[F]

    case class IntLiteral[F[_]](value: F[Int]) extends AST[F]
    case class StringLiteral[F[_]](value: F[String]) extends AST[F]

// case class WithSource[A](
//   value: A,
//   tokens: List[String],
// )

  }

}
