package playground.smithyql

import cats.Apply
import cats.Eval
import cats.Id
import cats.NonEmptyTraverse
import cats.Show
import cats.implicits._
import cats.kernel.Eq
import cats.~>

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

object SourceRange {
  implicit val show: Show[SourceRange] = Show.fromToString
}

final case class WithSource[+A](
  commentsLeft: List[Comment],
  commentsRight: List[Comment],
  range: SourceRange,
  value: A,
) {
  def allComments(valueComments: A => List[Comment]): List[Comment] =
    commentsLeft ++ valueComments(value) ++ commentsRight

  def withRange(range: SourceRange): WithSource[A] = copy(range = range)
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

  def allQueryComments(q: Query[WithSource]): List[Comment] = {

    def comments(node: InputNode[WithSource]): List[Comment] = node.fold(
      struct =
        _.fields.allComments(
          _.value
            .flatMap { case (k, v) =>
              k.allComments(_ => Nil) ++ v.allComments(
                _.fold(comments, comments, comments, comments, comments)
              )
            }
            .toList
        ),
      string = _ => Nil,
      int = _ => Nil,
      bool = _ => Nil,
      listed = _.values.allComments(_.flatMap(_.allComments(comments))),
    )

    q.useClause.foldMap(u => u.commentsLeft ++ u.commentsRight) ++
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
