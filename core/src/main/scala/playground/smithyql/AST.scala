package playground.smithyql

import cats.Functor
import cats.implicits._
import cats.~>

sealed trait AST[F[_]] extends Product with Serializable {
  def mapK[G[_]: Functor](fk: F ~> G): AST[G]
  def kind: NodeKind
}

sealed trait InputNode[F[_]] extends AST[F] {

  def fold[A](
    struct: Struct[F] => A,
    string: StringLiteral[F] => A,
    int: IntLiteral[F] => A,
    listed: Listed[F] => A,
    bool: BooleanLiteral[F] => A,
  ): A =
    this match {
      case s @ Struct(_)         => struct(s)
      case i @ IntLiteral(_)     => int(i)
      case b @ BooleanLiteral(_) => bool(b)
      case s @ StringLiteral(_)  => string(s)
      case l @ Listed(_)         => listed(l)
    }

  def mapK[G[_]: Functor](fk: F ~> G): InputNode[G]
}

final case class OperationName(text: String) extends AnyVal

final case class Query[F[_]](
  operationName: F[OperationName],
  input: F[Struct[F]],
) extends AST[F] {

  def kind: NodeKind = NodeKind.Query

  def mapK[G[_]: Functor](fk: F ~> G): Query[G] = Query(
    fk(operationName),
    fk(input).map(_.mapK(fk)),
  )

}

final case class Struct[F[_]](
  // todo: keep ordering of fields? Map might not be the thing to use
  fields: F[Map[F[Struct.Key], F[InputNode[F]]]]
) extends InputNode[F] {

  def kind: NodeKind = NodeKind.Struct

  def mapK[G[_]: Functor](fk: F ~> G): Struct[G] = Struct(
    fk(fields).map(_.map { case (k, v) => fk(k) -> fk(v).map(_.mapK(fk)) })
  )

}

object Struct {
  final case class Key(text: String) extends AnyVal
}

final case class IntLiteral[F[_]](value: Int) extends InputNode[F] {
  def kind: NodeKind = NodeKind.IntLiteral
  def mapK[G[_]: Functor](fk: F ~> G): InputNode[G] = copy()
}

final case class StringLiteral[F[_]](value: String) extends InputNode[F] {
  def kind: NodeKind = NodeKind.StringLiteral
  def mapK[G[_]: Functor](fk: F ~> G): InputNode[G] = copy()
}

final case class Listed[F[_]](values: F[List[InputNode[F]]]) extends InputNode[F] {
  def kind: NodeKind = NodeKind.Listed

  def mapK[G[_]: Functor](
    fk: F ~> G
  ): InputNode[G] = copy(values = fk(values).map(_.map(_.mapK(fk))))

}

final case class BooleanLiteral[F[_]](value: Boolean) extends InputNode[F] {
  def kind: NodeKind = NodeKind.Bool

  def mapK[G[_]: Functor](fk: F ~> G): InputNode[G] = BooleanLiteral(value)
}

sealed trait NodeKind extends Product with Serializable

object NodeKind {
  case object Struct extends NodeKind
  case object IntLiteral extends NodeKind
  case object StringLiteral extends NodeKind
  case object Query extends NodeKind
  case object Listed extends NodeKind
  case object Bool extends NodeKind
}
