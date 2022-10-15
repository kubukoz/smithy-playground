package playground.smithyql

import cats.Applicative
import cats.Apply
import cats.Functor
import cats.Id
import cats.Show
import cats.data.NonEmptyList
import cats.implicits._
import cats.kernel.Eq
import cats.kernel.Order
import cats.~>

sealed trait AST[F[_]] extends Product with Serializable {
  def mapK[G[_]: Functor](fk: F ~> G): AST[G]
}

object AST {
  implicit val astIdEq: Eq[AST[Id]] = Eq.fromUniversalEquals
}

sealed trait InputNode[F[_]] extends AST[F] {
  def kind: NodeKind

  def fold[A](
    struct: Struct[F] => A,
    string: StringLiteral[F] => A,
    int: IntLiteral[F] => A,
    listed: Listed[F] => A,
    bool: BooleanLiteral[F] => A,
    nul: NullLiteral[F] => A,
  ): A =
    this match {
      case s @ Struct(_)         => struct(s)
      case i @ IntLiteral(_)     => int(i)
      case b @ BooleanLiteral(_) => bool(b)
      case s @ StringLiteral(_)  => string(s)
      case l @ Listed(_)         => listed(l)
      case n @ NullLiteral()     => nul(n)
    }

  def mapK[G[_]: Functor](fk: F ~> G): InputNode[G]
}

final case class OperationName[F[_]](text: String) extends AST[F] {
  def mapK[G[_]: Functor](fk: F ~> G): OperationName[G] = copy()
}

final case class QualifiedIdentifier(segments: NonEmptyList[String], selection: String) {
  def renderNamespace: String = segments.mkString_(".")
  def render: String = renderNamespace + "#" + selection
}

object QualifiedIdentifier {

  def of(first: String, second: String, rest: String*): QualifiedIdentifier = {
    val all = first :: second :: rest.toList

    apply(NonEmptyList.fromListUnsafe(all.dropRight(1)), all.last)
  }

  implicit val show: Show[QualifiedIdentifier] = Show.fromToString

  implicit val ord: Order[QualifiedIdentifier] = Order.by { case QualifiedIdentifier(segs, sel) =>
    (segs, sel)
  }

}

final case class UseClause[F[_]](identifier: F[QualifiedIdentifier]) extends AST[F] {
  def mapK[G[_]: Functor](fk: F ~> G): UseClause[G] = UseClause(fk(identifier))
}

final case class QueryOperationName[F[_]](
  identifier: Option[F[QualifiedIdentifier]],
  operationName: F[OperationName[F]],
) extends AST[F] {

  def mapK[G[_]: Functor](fk: F ~> G): QueryOperationName[G] = QueryOperationName(
    identifier.map(fk(_)),
    fk(operationName).map(_.mapK(fk)),
  )

}

final case class Query[F[_]](
  useClause: F[Option[UseClause[F]]],
  operationName: F[QueryOperationName[F]],
  input: F[Struct[F]],
) extends AST[F] {

  def mapK[G[_]: Functor](fk: F ~> G): Query[G] = Query(
    fk(useClause).map(_.map(_.mapK(fk))),
    fk(operationName).map(_.mapK(fk)),
    fk(input).map(_.mapK(fk)),
  )

  def collectServiceIdentifiers(implicit F: Apply[F]): F[List[F[QualifiedIdentifier]]] =
    (
      useClause.map(_.map(_.identifier)),
      operationName.map(_.identifier),
    ).mapN(_.toList ++ _.toList)

}

final case class Struct[F[_]](
  fields: F[Struct.Fields[F]]
) extends InputNode[F] {

  def kind: NodeKind = NodeKind.Struct

  def mapK[G[_]: Functor](fk: F ~> G): Struct[G] = Struct(
    fk(fields).map(_.mapK(fk))
  )

}

final case class Binding[F[_]](identifier: F[Identifier], value: F[InputNode[F]]) {

  def mapK[G[_]: Functor](fk: F ~> G): Binding[G] = Binding(
    fk(identifier),
    fk(value).map(_.mapK(fk)),
  )

}

final case class Identifier(text: String) extends AnyVal

object Struct {

  final case class Fields[F[_]](value: List[Binding[F]]) {
    def keys: List[F[Identifier]] = value.map(_.identifier)

    def size: Int = value.size
    def head: Binding[F] = value.head
    def isEmpty: Boolean = value.isEmpty

    def mapK[G[_]: Functor](fk: F ~> G): Fields[G] = Fields(value.map(_.mapK(fk)))

    def keySet(getValue: F[Identifier] => Identifier): Set[String] =
      value.map(_.identifier).map(getValue).map(_.text).toSet

    // Usage not recommended, fields can have duplicate fields at the parsing stage
    def toMap: Map[F[Identifier], F[InputNode[F]]] = value.map(b => (b.identifier, b.value)).toMap

    def byName(
      name: String
    )(
      getValue: F[Identifier] => Identifier
    ): Option[F[InputNode[F]]] = value
      .find(pair => getValue(pair.identifier).text == name)
      .map(_.value)

  }

  def one[F[_]: Applicative](key: F[Identifier], value: F[InputNode[F]]): Struct[F] = Struct(
    Fields(List(Binding(key, value))).pure[F]
  )

  object Fields {

    def fromSeq[F[_]](value: Seq[Binding[F]]): Fields[F] = Fields(
      value.toList
    )

    def empty[F[_]]: Fields[F] = Fields(List.empty)

  }

}

final case class NullLiteral[F[_]]() extends InputNode[F] {
  def kind: NodeKind = NodeKind.NullLiteral
  def mapK[G[_]: Functor](fk: F ~> G): InputNode[G] = copy()
}

final case class IntLiteral[F[_]](value: String) extends InputNode[F] {
  def kind: NodeKind = NodeKind.IntLiteral
  def mapK[G[_]: Functor](fk: F ~> G): InputNode[G] = copy()
}

final case class StringLiteral[F[_]](value: String) extends InputNode[F] {
  def kind: NodeKind = NodeKind.StringLiteral
  def mapK[G[_]: Functor](fk: F ~> G): InputNode[G] = copy()
}

final case class Listed[F[_]](values: F[List[F[InputNode[F]]]]) extends InputNode[F] {
  def kind: NodeKind = NodeKind.Listed

  def mapK[G[_]: Functor](
    fk: F ~> G
  ): InputNode[G] = copy(values = fk(values).map(_.map(fk(_).map(_.mapK(fk)))))

}

final case class BooleanLiteral[F[_]](value: Boolean) extends InputNode[F] {
  def kind: NodeKind = NodeKind.Bool

  def mapK[G[_]: Functor](fk: F ~> G): InputNode[G] = BooleanLiteral(value)
}

sealed trait NodeKind extends Product with Serializable

object NodeKind {
  case object Struct extends NodeKind
  case object IntLiteral extends NodeKind
  case object NullLiteral extends NodeKind
  case object StringLiteral extends NodeKind
  case object Listed extends NodeKind
  case object Bool extends NodeKind
}