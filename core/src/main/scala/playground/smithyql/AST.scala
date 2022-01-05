package playground.smithyql

import cats.Functor
import cats.Id
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

      def fold[A](
        struct: Struct[F] => A,
        string: StringLiteral[F] => A,
        int: IntLiteral[F] => A,
      ): A =
        this match {
          case s @ Struct(_)        => struct(s)
          case i @ IntLiteral(_)    => int(i)
          case s @ StringLiteral(_) => string(s)
        }

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

}
