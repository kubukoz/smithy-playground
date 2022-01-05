package playground

import cats.Apply
import cats.Eval
import cats.Functor
import cats.Id
import cats.NonEmptyTraverse
import cats.implicits._
import cats.~>

object AST {

  type AST = AST.high.AST[Id]

  type Query = AST.high.Query[Id]
  val Query = AST.high.Query

  type Struct = AST.high.Struct[Id]
  val Struct = AST.high.Struct

  val IntLiteral = AST.high.IntLiteral

  val StringLiteral = AST.high.StringLiteral

  object high {

    sealed trait AST[F[_]] extends Product with Serializable {
      def mapK[G[_]: Functor](fk: F ~> G): AST[G]
    }

    sealed trait InputNode[F[_]] extends AST[F] {
      def mapK[G[_]: Functor](fk: F ~> G): InputNode[G]
    }

    final case class Query[F[_]](
      operationName: F[String],
      input: Struct[F],
    ) {

      def mapK[G[_]: Functor](fk: F ~> G): Query[G] = Query(
        fk(operationName),
        input.mapK(fk),
      )

    }

    final case class Struct[F[_]](
      fields: F[Map[F[String], InputNode[F]]]
    ) extends InputNode[F] {

      def mapK[G[_]: Functor](fk: F ~> G): Struct[G] = Struct(
        fk(fields).map(_.map { case (k, v) => fk(k) -> v.mapK(fk) })
      )

    }

    final case class IntLiteral[F[_]](value: F[Int]) extends InputNode[F] {
      def mapK[G[_]: Functor](fk: F ~> G): InputNode[G] = IntLiteral(fk(value))
    }

    final case class StringLiteral[F[_]](value: F[String]) extends InputNode[F] {
      def mapK[G[_]: Functor](fk: F ~> G): InputNode[G] = StringLiteral(fk(value))
    }

  }

  // todo: multiline
  final case class Comment(text: String)

  final case class WithSource[+A](
    tokensLeft: List[Comment],
    value: A,
    tokensRight: List[Comment],
  ) {

    def tokensEach: List[Comment] = tokensLeft ++ tokensRight

    def between(
      lhs: List[Comment],
      rhs: List[Comment],
    ) = WithSource(lhs ++ tokensLeft, value, tokensRight ++ rhs)

  }

  object WithSource {
    def empty[A](value: A) = WithSource(Nil, value, Nil)

    implicit val instances: NonEmptyTraverse[WithSource] =
      new NonEmptyTraverse[WithSource] {

        def foldLeft[A, B](fa: WithSource[A], b: B)(f: (B, A) => B): B = f(b, fa.value)

        def foldRight[A, B](fa: WithSource[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
          f(fa.value, lb)

        def reduceLeftTo[A, B](fa: WithSource[A])(f: A => B)(g: (B, A) => B): B = f(fa.value)

        def reduceRightTo[A, B](fa: WithSource[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
          Eval.later(f(fa.value))

        def nonEmptyTraverse[G[_]: Apply, A, B](fa: WithSource[A])(f: A => G[B]): G[WithSource[B]] =
          f(fa.value).map(v => fa.copy(value = v))

      }

    val unwrap: WithSource ~> Id =
      new (WithSource ~> Id) {
        def apply[A](wa: WithSource[A]): A = wa.value
      }

  }

}
