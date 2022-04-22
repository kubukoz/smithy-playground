package playground.smithyql

import cats.Functor
import cats.implicits._
import cats.~>
import cats.Applicative
import cats.data.NonEmptyList
import smithy4s.ShapeId

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

final case class QualifiedIdentifier(segments: NonEmptyList[String], selection: String) {
  def render: String = segments.mkString_(".") + "#" + selection
}

object QualifiedIdentifier {

  def fromShapeId(shapeId: ShapeId): QualifiedIdentifier = QualifiedIdentifier(
    shapeId.namespace.split("\\.").toList.toNel.getOrElse(sys.error("impossible! " + shapeId)),
    shapeId.name,
  )

}

//todo: identifier must be in WithSource
final case class UseClause(identifier: QualifiedIdentifier) extends AnyVal

final case class Query[F[_]](
  useClause: Option[F[UseClause]],
  operationName: F[OperationName],
  input: F[Struct[F]],
) extends AST[F] {

  def kind: NodeKind = NodeKind.Query

  def mapK[G[_]: Functor](fk: F ~> G): Query[G] = Query(
    useClause.map(fk(_)),
    fk(operationName),
    fk(input).map(_.mapK(fk)),
  )

}

final case class Struct[F[_]](
  fields: F[Struct.Fields[F]]
) extends InputNode[F] {

  def kind: NodeKind = NodeKind.Struct

  def mapK[G[_]: Functor](fk: F ~> G): Struct[G] = Struct(
    fk(fields).map(_.mapK(fk))
  )

}

object Struct {
  final case class Key(text: String) extends AnyVal

  final case class Fields[F[_]](value: List[(F[Struct.Key], F[InputNode[F]])]) {
    def keys: List[F[Struct.Key]] = value.map(_._1)

    def size: Int = value.size
    def head: (F[Struct.Key], F[InputNode[F]]) = value.head
    def isEmpty: Boolean = value.isEmpty

    def mapK[G[_]: Functor](fk: F ~> G): Fields[G] = Fields(value.map { case (k, v) =>
      fk(k) -> fk(v).map(_.mapK(fk))
    })

    def keySet(getValue: F[Struct.Key] => Struct.Key): Set[String] =
      value.map(_._1).map(getValue).map(_.text).toSet

    // Usage not recommended, fields can have duplicate fields at the parsing stage
    def toMap: Map[F[Struct.Key], F[InputNode[F]]] = value.toMap

    def byName(
      name: String
    )(
      getValue: F[Struct.Key] => Struct.Key
    ): Option[F[InputNode[F]]] = value.find(pair => getValue(pair._1).text == name).map(_._2)

  }

  def one[F[_]: Applicative](key: F[Struct.Key], value: F[InputNode[F]]): Struct[F] = Struct(
    Fields(List((key, value))).pure[F]
  )

  object Fields {

    def fromSeq[F[_]](value: Seq[(F[Struct.Key], F[InputNode[F]])]): Fields[F] = Fields(
      value.toList
    )

    def empty[F[_]]: Fields[F] = Fields(List.empty)

  }

}

final case class IntLiteral[F[_]](value: Int) extends InputNode[F] {
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
  case object StringLiteral extends NodeKind
  case object Query extends NodeKind
  case object Listed extends NodeKind
  case object Bool extends NodeKind
}
