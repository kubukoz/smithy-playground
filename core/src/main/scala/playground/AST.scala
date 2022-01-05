package playground

import cats.Apply
import cats.Eval
import cats.Functor
import cats.Id
import cats.NonEmptyTraverse
import cats.data.Writer
import cats.implicits._
import cats.~>

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

    final case class Query[F[_]](
      operationName: F[String],
      input: F[Struct[F]],
    ) {

      def mapK[G[_]: Functor](fk: F ~> G): Query[G] = Query(
        fk(operationName),
        fk(input).map(_.mapK(fk)),
      )

    }

// todo bad name, Query isn't an AST but is part of the AST
    sealed trait AST[F[_]] extends Product with Serializable {
      def mapK[G[_]: Functor](fk: F ~> G): AST[G]
    }

    final case class Struct[F[_]](
      fields: F[Map[F[String], F[AST[F]]]]
    ) extends AST[F] {

      def mapK[G[_]: Functor](fk: F ~> G): Struct[G] = Struct(
        fk(fields).map(_.map { case (k, v) => fk(k) -> fk(v).map(_.mapK(fk)) })
      )

    }

    final case class IntLiteral[F[_]](value: F[Int]) extends AST[F] {
      def mapK[G[_]: Functor](fk: F ~> G): AST[G] = IntLiteral(fk(value))
    }

    final case class StringLiteral[F[_]](value: F[String]) extends AST[F] {
      def mapK[G[_]: Functor](fk: F ~> G): AST[G] = StringLiteral(fk(value))
    }

  }

  sealed trait Token extends Product with Serializable

  object Token {
    case object LeftBrace extends Token
    case object RightBrace extends Token
    case object EqualsSign extends Token
    case object Comma extends Token
    // todo: multiline
    final case class Comment(text: String) extends Token
    final case class Identifier(value: String) extends Token
    final case class Literal(value: ALiteral) extends Token
  }

  sealed trait ALiteral extends Product with Serializable

  object ALiteral {
    final case class StringLiteral(value: String) extends ALiteral
    final case class IntLiteral(value: String) extends ALiteral
  }

  // this is basically Writer
  final case class WithSource[+A](
    value: A,
    tokens: List[Token],
  )

  object WithSource {
    Writer

    implicit val instances: NonEmptyTraverse[WithSource] with Apply[WithSource] =
      new NonEmptyTraverse[WithSource] with Apply[WithSource] {

        def ap[A, B](ff: WithSource[A => B])(fa: WithSource[A]): WithSource[B] = WithSource(
          ff.value(fa.value),
          ff.tokens ++ fa.tokens,
        )

        def foldLeft[A, B](fa: WithSource[A], b: B)(f: (B, A) => B): B = f(b, fa.value)

        def foldRight[A, B](fa: WithSource[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
          f(fa.value, lb)

        def reduceLeftTo[A, B](fa: WithSource[A])(f: A => B)(g: (B, A) => B): B = f(fa.value)

        def reduceRightTo[A, B](fa: WithSource[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
          Eval.later(f(fa.value))

        def nonEmptyTraverse[G[_]: Apply, A, B](fa: WithSource[A])(f: A => G[B]): G[WithSource[B]] =
          f(fa.value).map(WithSource(_, fa.tokens))

      }

    val unwrap: WithSource ~> Id =
      new (WithSource ~> Id) {
        def apply[A](wa: WithSource[A]): A = wa.value
      }

  }

}
