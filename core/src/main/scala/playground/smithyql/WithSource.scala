package playground.smithyql

import cats.Apply
import cats.Eval
import cats.Id
import cats.NonEmptyTraverse
import cats.implicits._
import cats.~>
import cats.kernel.Eq

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

  sealed trait Thing
  case class OperationThing(opName: WithSource[OperationName]) extends Thing

  case class StructThing(
    // why do we need this?
    content: WithSource[Map[WithSource[Struct.Key], WithSource[InputNode[WithSource]]]],
    context: List[String],
  ) extends Thing

  def atPosition(q: Query[WithSource])(pos: Position): Option[Thing] = {
    val op =
      if (q.operationName.range.contains(pos))
        OperationThing(q.operationName).some
      else
        None

    val input = findInStruct(q.input, pos)

    op.orElse(input)
  }

  private def findInStruct(struct: WithSource[Struct[WithSource]], pos: Position): Option[Thing] =
    struct
      .value
      .fields
      .value
      // find the first field whose value we're in
      .collectFirst {
        case (k, v) if v.range.contains(pos) && v.value.kind == NodeKind.Struct =>
          StructThing(v.value.asInstanceOf[Struct[WithSource]].fields, k.value.text :: Nil)
      }
      // we're only in the struct
      .orElse {
        if (struct.range.contains(pos))
          StructThing(struct.value.fields, Nil).some
        else
          None
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
