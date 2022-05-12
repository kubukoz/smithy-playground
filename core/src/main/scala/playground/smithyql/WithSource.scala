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
  sealed trait NodeContext extends Product with Serializable

  object NodeContext {
    final case class OperationContext(opName: WithSource[OperationName]) extends NodeContext

    final case class InputContext(context: Chain[PathEntry]) extends NodeContext {
      def append(elem: PathEntry) = copy(context.append(elem))

      def toList = context.toList
    }

    object InputContext {
      val root: InputContext = InputContext(Chain.nil)
    }

    sealed trait PathEntry extends Product with Serializable

    object PathEntry {
      final case class StructValue(key: String) extends PathEntry
    }

  }

  def atPosition(q: Query[WithSource])(pos: Position): Option[NodeContext] = {
    val op =
      if (q.operationName.range.contains(pos))
        NodeContext.OperationContext(q.operationName).some
      else
        None

    val input = findInNode(q.input, pos, NodeContext.InputContext.root)

    op.orElse(input)
  }

  private def findInNode(
    node: WithSource[InputNode[WithSource]],
    pos: Position,
    ctx: NodeContext.InputContext,
  ): Option[NodeContext] = {
    val entireNode = Option.when(node.range.contains(pos))(ctx)

    val inside =
      node.value match {
        case l @ Listed(_) => findInList(node.copy(value = l), pos, ctx)
        case s @ Struct(_) => findInStruct(node.copy(value = s), pos, ctx)
        case _             => None // not supported yet
      }

    inside
      .orElse(entireNode)
  }

  private def findInList(
    list: WithSource[Listed[WithSource]],
    pos: Position,
    ctx: NodeContext.InputContext,
  ): Option[NodeContext] = list
    .value
    .values
    .value
    .find(_.range.contains(pos))
    .flatMap(findInNode(_, pos, ctx))

  private def findInStruct(
    struct: WithSource[Struct[WithSource]],
    pos: Position,
    ctx: NodeContext.InputContext,
  ): Option[NodeContext] =
    // Struct fields that allow nesting in them
    struct
      .value
      .fields
      .value
      .value
      .view
      .map { case (k, v) =>
        findInNode(v, pos, ctx.append(NodeContext.PathEntry.StructValue(k.value.text)))
      }
      .flatten
      .headOption

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
