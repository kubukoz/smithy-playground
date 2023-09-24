package playground.smithyql

import cats.Applicative
import cats.Functor
import cats.Id
import cats.Show
import cats.data.NonEmptyList
import cats.implicits._
import cats.kernel.Eq
import cats.kernel.Order
import cats.~>

/** The main type for AST nodes of SmithyQL. The type parameter `F[_]` is a type constructor that
  * shall be used whenever source information may be captured. For example, WithSource[A] is a
  * wrapper that is used with AST nodes to capture range and comment information during parsing.
  *
  * F shall be used to wrap children only if it has any tokens of its own (using the term "token"
  * loosely to mean anything that isn't included in the children's ranges). For example, a string
  * literal might wrap its value in F to preserve the exact style of quotes being used. Or a
  * structure literal might wrap its members in F to keep the range enclosed by the braces (in fact,
  * that's precisely what's happening with SmithyQL structures).
  *
  * Notably, an AST node in this design shall (unless we screw up) never include the comments or
  * whitespace surrounding it. If a node can have comments around it, that's a concern of whatever
  * contains that node and that parent node should wrap the child in F.
  */
sealed trait AST[F[_]] extends Product with Serializable {

  def mapK[G[_]: Functor](
    fk: F ~> G
  ): AST[G]

}

object AST {
  implicit val astIdEq: Eq[AST[Id]] = Eq.fromUniversalEquals
}

sealed trait Statement[F[_]] extends AST[F] {

  def mapK[G[_]: Functor](
    fk: F ~> G
  ): Statement[G]

  def fold[B](
    runQuery: RunQuery[F] => B
  ): B =
    this match {
      case x: RunQuery[F] => runQuery(x)
    }

}

// corresponds to a "run ..." expression, currently without the `run` (see https://github.com/kubukoz/smithy-playground/discussions/83 for proposal).
final case class RunQuery[F[_]](
  // This F shall include any keyword used to "run" this query, if any.
  query: F[Query[F]]
) extends Statement[F] {

  def mapK[G[_]: Functor](
    fk: F ~> G
  ): RunQuery[G] = RunQuery(fk(query).map(_.mapK(fk)))

}

final case class Prelude[F[_]](
  useClauses: List[F[UseClause[F]]]
) extends AST[F] {

  def mapK[G[_]: Functor](
    fk: F ~> G
  ): Prelude[G] = Prelude(
    useClauses.map(fk(_).map(_.mapK(fk)))
  )

}

final case class SourceFile[F[_]](
  prelude: Prelude[F],
  // we have F here because files without statements / an import clause should be allowed to have comments
  statements: F[List[Statement[F]]],
) extends AST[F] {

  def queries(
    unwrapF: F ~> Id
  ): List[RunQuery[F]] = unwrapF(statements).flatMap(_.fold(_.some))

  def mapK[G[_]: Functor](
    fk: F ~> G
  ): AST[G] = SourceFile(
    prelude = prelude.mapK(fk),
    statements = fk(statements).map(_.map(_.mapK(fk))),
  )

}

object SourceFile {

  def fromSingleQuery[F[_]: Applicative](
    q: F[Query[F]]
  ): SourceFile[F] = SourceFile(Prelude[F](Nil), List[Statement[F]](RunQuery(q)).pure[F])

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

  def mapK[G[_]: Functor](
    fk: F ~> G
  ): InputNode[G]

}

final case class OperationName[F[_]](
  text: String
) extends AST[F] {

  def mapK[G[_]: Functor](
    fk: F ~> G
  ): OperationName[G] = copy()

}

object OperationName {

  implicit def order[F[_]]: Order[OperationName[F]] = Order.by(_.text)

}

final case class QualifiedIdentifier(
  segments: NonEmptyList[String],
  selection: String,
) {
  def renderNamespace: String = segments.mkString_(".")
  def render: String = renderNamespace + "#" + selection
}

object QualifiedIdentifier {

  def of(
    first: String,
    second: String,
    rest: String*
  ): QualifiedIdentifier = {
    val all = first :: second :: rest.toList

    apply(NonEmptyList.fromListUnsafe(all.dropRight(1)), all.last)
  }

  implicit val show: Show[QualifiedIdentifier] = Show.fromToString

  implicit val ord: Order[QualifiedIdentifier] = Order.by(unapply(_).get)

}

// the keywords of the clause are captured in the Prelude's useClauses list.
final case class UseClause[F[_]](
  identifier: F[QualifiedIdentifier]
) extends AST[F] {

  def mapK[G[_]: Functor](
    fk: F ~> G
  ): UseClause[G] = UseClause(fk(identifier))

}

final case class QueryOperationName[F[_]](
  identifier: Option[F[QualifiedIdentifier]],
  operationName: F[OperationName[F]],
) extends AST[F] {

  def mapK[G[_]: Functor](
    fk: F ~> G
  ): QueryOperationName[G] = QueryOperationName(
    identifier.map(fk(_)),
    fk(operationName).map(_.mapK(fk)),
  )

}

final case class Query[F[_]](
  operationName: F[QueryOperationName[F]],
  input: F[Struct[F]],
) extends AST[F] {

  def mapK[G[_]: Functor](
    fk: F ~> G
  ): Query[G] = Query(
    fk(operationName).map(_.mapK(fk)),
    fk(input).map(_.mapK(fk)),
  )

}

final case class Struct[F[_]](
  // F includes braces
  fields: F[Struct.Fields[F]]
) extends InputNode[F] {

  def kind: NodeKind = NodeKind.Struct

  def mapK[G[_]: Functor](
    fk: F ~> G
  ): Struct[G] = Struct(
    fk(fields).map(_.mapK(fk))
  )

}

final case class Binding[F[_]](
  identifier: F[Identifier],
  value: F[InputNode[F]],
) {

  def mapK[G[_]: Functor](
    fk: F ~> G
  ): Binding[G] = Binding(
    fk(identifier),
    fk(value).map(_.mapK(fk)),
  )

}

final case class Identifier(
  text: String
) extends AnyVal

object Struct {

  // todo: wrap each binding in F
  final case class Fields[F[_]](
    value: List[Binding[F]]
  ) {
    def keys: List[F[Identifier]] = value.map(_.identifier)

    def size: Int = value.size
    def head: Binding[F] = value.head
    def isEmpty: Boolean = value.isEmpty

    def mapK[G[_]: Functor](
      fk: F ~> G
    ): Fields[G] = Fields(value.map(_.mapK(fk)))

    def keySet(
      getValue: F[Identifier] => Identifier
    ): Set[String] = value.map(_.identifier).map(getValue).map(_.text).toSet

    // Usage not recommended, fields can have duplicate fields at the parsing stage
    def toMap: Map[F[Identifier], F[InputNode[F]]] = value.map(b => (b.identifier, b.value)).toMap

    def byName(
      name: String
    )(
      getValue: F[Identifier] => Identifier
    ): Option[F[InputNode[F]]] = value
      .find(pair => getValue(pair.identifier).text === name)
      .map(_.value)

  }

  def one[F[_]: Applicative](
    key: F[Identifier],
    value: F[InputNode[F]],
  ): Struct[F] = Struct(
    Fields(List(Binding(key, value))).pure[F]
  )

  object Fields {

    def fromSeq[F[_]](
      value: Seq[Binding[F]]
    ): Fields[F] = Fields(
      value.toList
    )

    def empty[F[_]]: Fields[F] = Fields(List.empty)

  }

}

final case class NullLiteral[F[_]](
) extends InputNode[F] {
  def kind: NodeKind = NodeKind.NullLiteral

  def mapK[G[_]: Functor](
    fk: F ~> G
  ): InputNode[G] = copy()

}

final case class IntLiteral[F[_]](
  value: String
) extends InputNode[F] {
  def kind: NodeKind = NodeKind.IntLiteral

  def mapK[G[_]: Functor](
    fk: F ~> G
  ): InputNode[G] = copy()

}

final case class StringLiteral[F[_]](
  value: String
) extends InputNode[F] {
  def kind: NodeKind = NodeKind.StringLiteral

  def mapK[G[_]: Functor](
    fk: F ~> G
  ): InputNode[G] = copy()

}

final case class Listed[F[_]](
  // F includes brackets
  values: F[List[F[InputNode[F]]]]
) extends InputNode[F] {
  def kind: NodeKind = NodeKind.Listed

  def mapK[G[_]: Functor](
    fk: F ~> G
  ): InputNode[G] = copy(values = fk(values).map(_.map(fk(_).map(_.mapK(fk)))))

}

final case class BooleanLiteral[F[_]](
  value: Boolean
) extends InputNode[F] {
  def kind: NodeKind = NodeKind.Bool

  def mapK[G[_]: Functor](
    fk: F ~> G
  ): InputNode[G] = BooleanLiteral(value)

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
