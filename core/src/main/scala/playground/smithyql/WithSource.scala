package playground.smithyql

import cats.Apply
import cats.Eval
import cats.Id
import cats.NonEmptyTraverse
import cats.data.Chain
import cats.implicits._
import cats.kernel.Eq
import cats.~>
import cats.Show

// todo: multiline
final case class Comment(text: String) extends AnyVal

object Comment {
  implicit val eq: Eq[Comment] = Eq.by(_.text)
}

final case class Position(index: Int)

object Position {
  val origin: Position = Position(index = 0)
}

final case class SourceRange(start: Position, end: Position) {
  def contains(pos: Position): Boolean = pos.index >= start.index && pos.index <= end.index

  // Assuming this range corresponds to a bracket/brace/quote etc.,
  // shrink it by one character on each side.
  def shrink1: SourceRange = copy(
    start = start.copy(index = start.index + 1),
    end = end.copy(index = end.index - 1),
  )

}

final case class WithSource[+A](
  commentsLeft: List[Comment],
  commentsRight: List[Comment],
  range: SourceRange,
  value: A,
) {
  def allComments(valueComments: A => List[Comment]): List[Comment] =
    commentsLeft ++ valueComments(value) ++ commentsRight
}

object WithSource {

  val liftId: Id ~> WithSource =
    new (Id ~> WithSource) {

      def apply[A](fa: Id[A]): WithSource[A] = WithSource(
        commentsLeft = Nil,
        commentsRight = Nil,
        range = SourceRange(Position(0), Position(0)),
        value = fa,
      )

    }

  implicit def showWithSource[A]: Show[WithSource[A]] = Show.fromToString

  // The path to a position in the parsed source
  sealed trait NodeContext extends Product with Serializable {
    def render: String
  }

  object NodeContext {

    final case class OperationContext(opName: WithSource[OperationName]) extends NodeContext {
      def render: String = ".operationName"
    }

    final case class InputContext(context: Chain[PathEntry]) extends NodeContext {
      def append(elem: PathEntry) = copy(context.append(elem))

      def toList = context.toList

      def render: String = {
        import PathEntry._
        context
          .map {
            case CollectionEntry  => ".[]"
            case StructValue(key) => s".$key"
            case StructBody       => ".{}"
            case Quotes           => ".\"\""
          }
          .mkString_(".input", "", "")
      }

    }

    object InputContext {
      val root: InputContext = InputContext(Chain.nil)
    }

    sealed trait PathEntry extends Product with Serializable

    object PathEntry {
      final case class StructValue(key: String) extends PathEntry
      case object StructBody extends PathEntry
      case object CollectionEntry extends PathEntry
      case object Quotes extends PathEntry
    }

  }

  def atPosition(q: Query[WithSource])(pos: Position): Option[NodeContext] = {
    val ctx = NodeContext.InputContext.root

    findInOperationName(q.operationName, pos)
      .orElse(findInNode(q.input, pos, ctx))
  }

  private def findInOperationName(
    operationName: WithSource[OperationName],
    pos: Position,
  ): Option[NodeContext.OperationContext] =
    if (operationName.range.contains(pos))
      NodeContext.OperationContext(operationName).some
    else
      None

  private def findInNode(
    node: WithSource[InputNode[WithSource]],
    pos: Position,
    ctx: NodeContext.InputContext,
  ): Option[NodeContext] = {
    def entireNode(ctx: NodeContext.InputContext) = Option.when(node.range.contains(pos))(ctx)

    node.value match {
      case l @ Listed(_) =>
        findInList(l, pos, ctx.append(NodeContext.PathEntry.CollectionEntry))
          .orElse(entireNode(ctx))

      case s @ Struct(_) =>
        findInStruct(s, pos, ctx.append(NodeContext.PathEntry.StructBody))
          .orElse(entireNode(ctx))

      case StringLiteral(_) =>
        val inQuotes = node
          .range
          .shrink1
          .contains(pos)
          .guard[Option]
          .as(
            ctx.append(NodeContext.PathEntry.Quotes)
          )

        inQuotes.orElse(entireNode(ctx))

      case _ =>
        // Default case: can be triggered e.g. inside a string literal
        // which would affect completions of enum values and timestamps.
        entireNode(ctx)
    }

  }

  private def findInList(
    list: Listed[WithSource],
    pos: Position,
    ctx: NodeContext.InputContext,
  ): Option[NodeContext] = {
    val inItems = list
      .values
      .value
      .find(_.range.contains(pos))
      .flatMap(findInNode(_, pos, ctx))

    val inBody = list
      .values
      .range
      .contains(pos)
      .guard[Option]
      .as(ctx)

    inItems
      .orElse(inBody)
  }

  private def findInStruct(
    struct: Struct[WithSource],
    pos: Position,
    ctx: NodeContext.InputContext,
  ): Option[NodeContext] =
    // Struct fields that allow nesting in them
    {
      val inFields =
        struct
          .fields
          .value
          .value
          .view
          .map { case (k, v) =>
            findInNode(v, pos, ctx.append(NodeContext.PathEntry.StructValue(k.value.text)))
          }
          .flatten
          .headOption

      val inBody = struct.fields.range.contains(pos).guard[Option].as(ctx)

      inFields
        .orElse(inBody)
    }

  def allQueryComments(q: Query[WithSource]): List[Comment] = {

    def comments(node: InputNode[WithSource]): List[Comment] = node.fold(
      struct =
        _.fields.allComments(_.value.flatMap { case (k, v) =>
          k.allComments(_ => Nil) ++ v.allComments(
            _.fold(comments, comments, comments, comments, comments)
          )
        }.toList),
      string = _ => Nil,
      int = _ => Nil,
      bool = _ => Nil,
      listed = _.values.allComments(_.flatMap(_.allComments(comments))),
    )

    q.operationName.allComments(_ => Nil) ++
      q.input
        .allComments(
          _.fold(
            comments,
            comments,
            comments,
            comments,
            comments,
          )
        )
  }

  implicit val instances: NonEmptyTraverse[WithSource] =
    new NonEmptyTraverse[WithSource] {

      def foldLeft[A, B](fa: WithSource[A], b: B)(f: (B, A) => B): B = f(b, fa.value)

      def foldRight[A, B](
        fa: WithSource[A],
        lb: Eval[B],
      )(
        f: (A, Eval[B]) => Eval[B]
      ): Eval[B] = f(fa.value, lb)

      def reduceLeftTo[A, B](fa: WithSource[A])(f: A => B)(g: (B, A) => B): B = f(fa.value)

      def reduceRightTo[A, B](fa: WithSource[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
        Eval.later(f(fa.value))

      def nonEmptyTraverse[G[_]: Apply, A, B](fa: WithSource[A])(f: A => G[B]): G[WithSource[B]] =
        f(fa.value).map(v => fa.copy(value = v))

    }

  val dropComments: WithSource ~> WithSource =
    new (WithSource ~> WithSource) {

      def apply[A](fa: WithSource[A]): WithSource[A] = fa.copy(
        commentsLeft = Nil,
        commentsRight = Nil,
      )

    }

  val unwrap: WithSource ~> Id =
    new (WithSource ~> Id) {
      def apply[A](wa: WithSource[A]): A = wa.value
    }

}
