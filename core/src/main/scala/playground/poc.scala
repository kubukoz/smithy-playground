package playground

import cats.implicits._
import cats.Id

object poc {

  case class Query[F[_]](
    operationName: F[String],
    input: F[Struct[F]],
  )

  // todo bad name, Query isn't an AST but is part of the AST
  sealed trait AST[F[_]] extends Product with Serializable

  case class Struct[F[_]](
    fields: F[Map[String, F[AST[F]]]]
  ) extends AST[F]

  case class IntLiteral[F[_]](value: F[Int]) extends AST[F]
  case class StringLiteral[F[_]](value: F[String]) extends AST[F]

  case class Tokens[A](before: List[A], own: List[A], after: List[A])

  case class WithSource[A](
    value: A,
    tokens: Tokens[String],
  )

  val parsedQuery = WithSource(
    Query[WithSource](
      operationName = WithSource("query", List("query")),
      input = WithSource(
        Struct[WithSource](
          fields = WithSource(
            Map(
              "name" -> WithSource(
                StringLiteral[WithSource](value = WithSource("John", List("\"John\""))),
                List("\"John\""),
              ),
              "age" -> WithSource(
                IntLiteral[WithSource](value = WithSource(10, List("10"))),
                List("10"),
              ),
            ),
            List("//foo", "name", "=", "\"John\"", "age", "=", "10"),
          )
        ),
        List("{", "//foo", "name", "=", "\"John\"", "age", "=", "10", "}"),
      ),
    ),
    List("query", "{", "//foo", "name", "=", "\"John\"", "age", "=", "10", "}"),
  )

}
