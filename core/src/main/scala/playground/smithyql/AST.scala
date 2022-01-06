package playground.smithyql

import cats.Functor
import cats.implicits._
import cats.~>

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
) extends AST[F] {

  def mapK[G[_]: Functor](fk: F ~> G): Query[G] = Query(
    fk(operationName),
    input.mapK(fk),
  )

}

// todo: experiment with "external" tokens outside of these nodes (extra F around InputNode in Struct, etc.)
final case class Struct[F[_]](
  // todo: keep ordering of fields? Map might not be the thing to use
  fields: F[F[Map[F[String], InputNode[F]]]]
) extends InputNode[F] {

  def mapK[G[_]: Functor](fk: F ~> G): Struct[G] = Struct(
    fk(fields).map(fk(_)).map(_.map(_.map { case (k, v) => fk(k) -> v.mapK(fk) }))
  )

}

final case class IntLiteral[F[_]](value: F[Int]) extends InputNode[F] {
  def mapK[G[_]: Functor](fk: F ~> G): InputNode[G] = IntLiteral(fk(value))
}

final case class StringLiteral[F[_]](value: F[String]) extends InputNode[F] {
  def mapK[G[_]: Functor](fk: F ~> G): InputNode[G] = StringLiteral(fk(value))
}
