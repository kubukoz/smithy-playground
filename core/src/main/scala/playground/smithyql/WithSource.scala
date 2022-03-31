package playground.smithyql

import cats.Apply
import cats.Eval
import cats.Id
import cats.NonEmptyTraverse
import cats.data.Chain
import cats.implicits._
import cats.kernel.Eq
import cats.~>

// todo: multiline
final case class Comment(text: String) extends AnyVal

object Comment {
  implicit val eq: Eq[Comment] = Eq.by(_.text)
}

final case class Position(index: Int)

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

  // The path to a position in the parsed source
  sealed trait NodeContext extends Product with Serializable
  final case class OperationContext(opName: WithSource[OperationName]) extends NodeContext
  final case class InputContext(context: List[String]) extends NodeContext

  def atPosition(q: Query[WithSource])(pos: Position): Option[NodeContext] = {
    val op =
      if (q.operationName.range.contains(pos))
        OperationContext(q.operationName).some
      else
        None

    val input = findInStruct(q.input, pos, Chain.nil)

    op.orElse(input)
  }

  private def findInStruct(
    struct: WithSource[Struct[WithSource]],
    pos: Position,
    ctx: Chain[String],
  ): Option[NodeContext] = {
    def recurse(
      k: WithSource[Struct.Key],
      v: WithSource[Struct[WithSource]],
    ): Option[NodeContext] = findInStruct(v, pos, ctx.append(k.value.text))

    // Struct fields that are also structs
    val structFields =
      struct
        .value
        .fields
        .value
        .view
        .map { case (k, v) =>
          v.traverse {
            case s @ Struct(_) => s.some
            case _             => none
          }.tupleLeft(k)
        }
        .flatten

    val matchingField = structFields
      .find { case (_, v) => v.range.contains(pos) }
      .flatMap(recurse.tupled)

    val self =
      Option.when(struct.range.contains(pos)) {
        InputContext(ctx.toList)
      }

    // possible optimization: don't even attempt to find matching fields
    // if we're not even in the struct range
    matchingField.orElse(self)
  }

  def allQueryComments(q: Query[WithSource]): List[Comment] = {

    def comments(node: InputNode[WithSource]): List[Comment] = node.fold(
      struct = _.fields.allComments(_.flatMap { case (k, v) =>
        k.allComments(_ => Nil) ++ v.allComments(_.fold(comments, comments, comments))
      }.toList),
      string = _ => Nil,
      int = _ => Nil,
    )

    q.operationName.allComments(_ => Nil) ++
      q.input
        .allComments(
          _.fold(
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
