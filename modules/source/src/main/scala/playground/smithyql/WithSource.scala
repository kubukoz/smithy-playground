package playground.smithyql

import cats.Apply
import cats.Eval
import cats.Id
import cats.NonEmptyTraverse
import cats.Show
import cats.kernel.Eq
import cats.kernel.Order
import cats.syntax.all.*
import cats.~>

// todo: multiline
final case class Comment(
  text: String
)

object Comment {
  implicit val eq: Eq[Comment] = Eq.by(_.text)
}

final case class Position(
  index: Int
) {

  def moveLeft(
    n: Int
  ): Position = Position(index - n)

  def moveRight(
    n: Int
  ): Position = Position(index + n)

}

object Position {
  val origin: Position = Position(0)

  def lastInString(
    s: String
  ): Position = Position(s.length)

  implicit val ord: Order[Position] = Order.by(_.index)
}

final case class SourceRange(
  start: Position,
  end: Position,
) {

  // Like a union, but includes the space between the ranges if present.
  def fakeUnion(
    another: SourceRange
  ): SourceRange =
    if (start.index <= another.start.index)
      SourceRange(start, another.end)
    else
      SourceRange(another.start, end)

  def contains(
    pos: Position
  ): Boolean = pos.index >= start.index && pos.index <= end.index

  // Assuming this range corresponds to a bracket/brace/quote etc.,
  // shrink it by one character on each side.
  def shrink1: SourceRange = copy(
    start = start.copy(index = start.index + 1),
    end = end.copy(index = end.index - 1),
  )

  def render: String = s"${start.index}-${end.index}"

}

object SourceRange {
  implicit val show: Show[SourceRange] = Show.fromToString
  implicit val ord: Order[SourceRange] = Order.by { case SourceRange(start, end) => (start, end) }

  def empty(
    position: Position
  ): SourceRange = SourceRange(position, position)

  def forEntireString(
    s: String
  ): SourceRange = SourceRange(Position.origin, Position.lastInString(s))

}

final case class WithSource[+A](
  commentsLeft: List[Comment],
  commentsRight: List[Comment],
  range: SourceRange,
  value: A,
) {

  def allComments(
    valueComments: A => List[Comment]
  ): List[Comment] = commentsLeft ++ valueComments(value) ++ commentsRight

  def withRange(
    range: SourceRange
  ): WithSource[A] = copy(range = range)

  def withComments(
    commentsLeft: List[Comment],
    commentsRight: List[Comment],
  ): WithSource[A] = copy(commentsLeft = commentsLeft, commentsRight = commentsRight)

}

object WithSource {

  val liftId: Id ~> WithSource =
    new (Id ~> WithSource) {

      def apply[A](
        fa: Id[A]
      ): WithSource[A] = WithSource(
        commentsLeft = Nil,
        commentsRight = Nil,
        range = SourceRange(Position(0), Position(0)),
        value = fa,
      )

    }

  implicit def showWithSource[A]: Show[WithSource[A]] = Show.fromToString

  def allSourceComments(
    sf: SourceFile[WithSource]
  ): List[Comment] =
    sf.prelude
      .useClauses
      .foldMap(
        _.allComments(uc =>
          uc
            .identifier
            .allComments(_ => Nil)
        )
      ) ++
      sf.statements.allComments(_.flatMap(allStatementComments))

  def allStatementComments(
    s: Statement[WithSource]
  ): List[Comment] = s.fold(_.query.allComments(allQueryComments))

  def allQueryComments(
    q: Query[WithSource]
  ): List[Comment] = {

    def comments(
      node: InputNode[WithSource]
    ): List[Comment] = node.fold(
      struct =
        _.fields.allComments(
          _.value
            .flatMap { binding =>
              binding.identifier.allComments(_ => Nil) ++ binding
                .value
                .allComments(
                  _.fold(comments, comments, comments, comments, comments, comments)
                )
            }
            .toList
        ),
      string = _ => Nil,
      int = _ => Nil,
      bool = _ => Nil,
      listed = _.values.allComments(_.flatMap(_.allComments(comments))),
      nul = _ => Nil,
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
            comments,
          )
        )
  }

  implicit val instances: NonEmptyTraverse[WithSource] =
    new NonEmptyTraverse[WithSource] {

      def foldLeft[A, B](
        fa: WithSource[A],
        b: B,
      )(
        f: (
          B,
          A,
        ) => B
      ): B = f(b, fa.value)

      def foldRight[A, B](
        fa: WithSource[A],
        lb: Eval[B],
      )(
        f: (
          A,
          Eval[B],
        ) => Eval[B]
      ): Eval[B] = f(fa.value, lb)

      def reduceLeftTo[A, B](
        fa: WithSource[A]
      )(
        f: A => B
      )(
        g: (
          B,
          A,
        ) => B
      ): B = f(fa.value)

      def reduceRightTo[A, B](
        fa: WithSource[A]
      )(
        f: A => B
      )(
        g: (
          A,
          Eval[B],
        ) => Eval[B]
      ): Eval[B] = Eval.later(f(fa.value))

      def nonEmptyTraverse[G[_]: Apply, A, B](
        fa: WithSource[A]
      )(
        f: A => G[B]
      ): G[WithSource[B]] = f(fa.value).map(v => fa.copy(value = v))

    }

  val unwrap: WithSource ~> Id =
    new (WithSource ~> Id) {

      def apply[A](
        wa: WithSource[A]
      ): A = wa.value

    }

}
